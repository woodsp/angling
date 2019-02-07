###
### Code to intersect angling trips with lake polygons
###   run this from the top level of the angling repository
###

library(lubridate)
library(rgdal)
library(raster)
library(rgeos)
library(sf)

## Depends on a custom NHD geodatabase
## ## called NHD_H_National_GDB_Waterbody_39000-46599.gpkg
## ## created from the NHD_H_National_GDB.gdb distributed by USGS
## ##   with just NHDWaterbody layer and features with Fcode between 39000-46599
## ## using ogr2ogr from the shell:
# ogr2ogr -f "GPKG" -sql "SELECT * FROM NHDWaterbody WHERE FCode >= 39000 AND FCode <= 46599" ./NHD_sql.gpkg /home/junk/NHD_H_National_GDB.gdb NHDWaterbody


## Define albers projection for all spatial functions
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

## Inputs are angling events and polygons of lakes
## Outputs are table of angling events by lake

## Import/Munge Angling
# Read angling point events
angling_hotspots_raw <- read.csv("./data/iBobber_Jan25_2019.csv", header=TRUE, na.strings=NULL, stringsAsFactors=F)
angling_hotspots <- angling_hotspots_raw

# Format timestamps
angling_hotspots$timestamp <- ymd_hms(angling_hotspots$timestamp, tz="UTC")
# split into cols for date and time (in Denver, mountain time)
angling_hotspots$date <- as.Date(with_tz(angling_hotspots$timestamp, tzone="America/Denver"))
angling_hotspots$hour <- hour(with_tz(angling_hotspots$timestamp, tzone="America/Denver"))

# drop extraneous variables
angling_hotspots <- angling_hotspots[,c("hotspot_id", "latitude", "longitude", "bobber_id", "date", "hour")]

# Subset 2017-2018 records (TEMPORARY?)
angling_hotspots <- angling_hotspots[which((angling_hotspots$date > "2016-12-31") & (angling_hotspots$date < "2019-01-01")), names(angling_hotspots) %in% c("hotspot_id", "latitude", "longitude", "bobber_id", "date", "hourMT")]

# Reprojecting angling events to Albers to match waterbodies
angling_hotspots_sp <- SpatialPointsDataFrame(angling_hotspots[ ,c("longitude","latitude")], angling_hotspots, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
angling_hotspots_sp <- spTransform(angling_hotspots_sp, CRS(albers))


## Import/Munge Waterbodies
# import state shapefile and reproject as required input
states <- readOGR("data/cb_2017_us_state_20m", "cb_2017_us_state_20m")
states <- spTransform(states, CRS(albers))

# loop through batches of waterbodies
#   this hacky code is slow-as!
#   the 'limit' variable sets the batch size, which affects memory requirement and speed
#   performance on shorts test runs w 2017-19 angling w limits 500, 1000, 4000 said lakes/s 
#   so a 1000 lake limit is probably most optimal

limit <- 1000
found <- limit
outlakes <- list();  outangle <- list();  cnt <- 0;  cntout <- 0
ptm <- proc.time()
while (found == limit) {
  for (offset in seq(0, 10000000, limit)) {
    cnt <- cnt+1

    # Import raw NHD spatial data from a custom gpkg version
    geosql <- paste("SELECT * FROM NHDWaterbody LIMIT", limit, "OFFSET", offset)
    print("elapsed time:"); print(proc.time() - ptm)
    print(paste("LOOP",cnt,geosql))
    NHD_raw <- st_read(dsn="./data/NHD_H_National_GDB_Waterbody_39000-46599.gpkg", query=geosql)

    # Lazy convert sf object back to SPDF (instead of recoding remaining fxns)
    NHD <- as_Spatial(st_zm(NHD_raw))

    # Reproject NHD polygons to albers
    NHD <- spTransform(NHD, CRS(albers))

    # Create lake centroid locations
    NHD_cent <- gCentroid(NHD, byid=T)
    NHD_cent <- spTransform(NHD_cent, CRS(albers))
    NHD_cent_wgs <- spTransform(NHD_cent, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))

    # Find closest state
    statedists <- gDistance(NHD_cent, states, byid=T)
    stateshort <- as.character(states@data$STUSPS)[apply(statedists, 2, which.min)]

    # Build waterbodies table
    outlakes[[cnt]] <- cbind.data.frame(PERMANENT_IDENTIFIER=as.character(NHD@data$PERMANENT_IDENTIFIER), state=stateshort, latitude_cent=NHD_cent_wgs@coords[ ,1], longitude_cent=NHD_cent_wgs@coords[ ,2])

    # Buffer NHD lakes by 50m
    NHD_buff <- gBuffer(NHD, width=50, byid=T)

    # Subset angling events within bounding box of NHD AOI
    angling_hotspots_sp_bb <- angling_hotspots_sp[angling_hotspots_sp$latitude > bbox(NHD_buff)[2] & angling_hotspots_sp$latitude < bbox(NHD_buff)[4] & angling_hotspots_sp$longitude > bbox(NHD_buff)[1] & angling_hotspots_sp$longitude < bbox(NHD_buff)[3], ]
    print(paste("   Found", nrow(angling_hotspots_sp_bb), "angling events in bbox of waterbodies"))

    # Intesect (add lake ID onto angling events; if there are any to intersect) and build angling table
    if (nrow(angling_hotspots_sp_bb) > 0) {
      angNwat <- over(angling_hotspots_sp_bb, NHD_buff)
      cntout <- cntout+1
      jnk <- cbind.data.frame(hotspot_id=angling_hotspots_sp_bb@data$hotspot_id, PERMANENT_IDENTIFIER=as.character(angNwat$PERMANENT_IDENTIFIER))
      outangle[[cntout]] <- jnk[!is.na(jnk$PERMANENT_IDENTIFIER), ]
    }

  # QAQC
#   plot(NHD_buff)
#   plot(NHD, add=T)
#   plot(angling_hotspots_sp_bb, add=T)

  # update the number found for while loop
  found <- nrow(NHD)

  }
}

## Reshape lake and angling lists into data frames
outlakesdf <- do.call("rbind",outlakes)
outangledf <- do.call("rbind",outangle)

## Export waterbody x state and angling by waterbody linking tables
write.csv(outlakesdf, "data/waterbody2state.csv", row.names=F)
write.csv(outangledf, "data/ibobber2waterbody.csv", row.names=F)

## Bind waterbody metadata onto angling events
ahp <- merge(angling_hotspots, outangledf, by="hotspot_id", all.x=T)
ahps <- merge(ahp, outlakesdf, by="PERMANENT_IDENTIFIER", all.x=T)

## Cleanup and Export
# Identified hotspots with duplicate bobber ID, lake, and date combinations to get trips
ahps$bdoID <- paste(ahps$bobber_id, ahps$PERMANENT_IDENTIFIER, ahps$date, sep='x')
ahps$uniq <- !duplicated(ahps$bdoID)

# Table for first round of analyses
toa <- ahps[ahps$uniq==T,c("bobber_id","date","PERMANENT_IDENTIFIER","latitude_cent","longitude_cent")]

# Write table to analyse
write.csv(toa, "./analysis/anglingXwater.csv", row.names=F)
