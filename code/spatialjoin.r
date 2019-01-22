###
### Code to intersect angling trips with lake polygons
###   run this from the top level of the angling repository
###

## Inputs are angling events and polygons of lakes
## Outputs are table of angling events by lake

## TODO: First version for WA.  Need to scale-up to nation.

## Import/Munge Angling
# Read angling point events
angling_hotspots_raw <- read.csv("./data/IB_Data_Mar18.csv", header=TRUE, na.strings=NULL, stringsAsFactors=F)
angling_hotspots <- angling_hotspots_raw

# Format timestamps
library(lubridate)
angling_hotspots$timestamp <- ymd_hms(angling_hotspots$timestamp, tz="UTC")
# split into cols for date and time (in Denver, mountain time)
angling_hotspots$date <- as.Date(with_tz(angling_hotspots$timestamp, tzone="America/Denver"))
angling_hotspots$hour <- hour(with_tz(angling_hotspots$timestamp, tzone="America/Denver"))

# drop extraneous variables
angling_hotspots <- angling_hotspots[,c("hotspot_id", "latitude", "longitude", "bobber_id", "date", "hour")]

# Subset 2017 records
angling_hotspots <- angling_hotspots[which((angling_hotspots$date > "2016-12-31") & (angling_hotspots$date < "2018-01-01")), names(angling_hotspots) %in% c("hotspot_id", "latitude", "longitude", "bobber_id", "date", "hourMT")]

# angling_hotspots <- angling_hotspots[angling_hotspots$latitude > 25 & angling_hotspots$latitude < 50 & angling_hotspots$longitude > -127 & angling_hotspots$longitude < -65, ]

## Import/Munge Waterbodies
# Read NHD spatial data
library(rgdal)
library(raster)
library(rgeos)
NHD_WA_raw <- readOGR(dsn="./data/", layer="NHDWaterbody_WA", stringsAsFactors=F)

# Filter NHD to lakes
NHD_WA <- NHD_WA_raw[which((NHD_WA_raw@data$FCode >= "39000") & (NHD_WA_raw@data$FCode <= "46599")), names(NHD_WA_raw@data) %in% c("OBJECTID", "Permanent_", "FDate", "Resolution", "GNIS_ID", "GNIS_Name", "AreaSqKm", "Elevation", "ReachCode", "FType", "FCode", "Visibility", "Shape_Leng", "Shape_Area")]

# Reproject NHD polygons to albers
NHD_WA <- spTransform(NHD_WA, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# Add lake centroid locations to NHD_WA
NHD_WA_cent <- gCentroid(NHD_WA, byid=T)
NHD_WA@data[ ,c("longitude_cent","latitude_cent")] <- coordinates(NHD_WA_cent)

# Buffer NHD lakes by 50m
NHD_WA_buff <- gBuffer(NHD_WA, width=50, byid=T)


## Intersect angling points with buffered lake polys
# Start by reprojecting angling events to Albers to match waterbodies
angling_hotspots_sp <- SpatialPointsDataFrame(angling_hotspots[ ,c("longitude","latitude")], angling_hotspots, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
angling_hotspots_sp <- spTransform(angling_hotspots_sp, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# Subset angling events within bounding box of NHD AOI
angling_hotspots_sp <- angling_hotspots_sp[angling_hotspots_sp$latitude > bbox(NHD_WA_buff)[2] & angling_hotspots_sp$latitude < bbox(NHD_WA_buff)[4] & angling_hotspots_sp$longitude > bbox(NHD_WA_buff)[1] & angling_hotspots_sp$longitude < bbox(NHD_WA_buff)[3], ]

# plot(NHD_WA_buff)
# plot(NHD_WA_cent)
# plot(angling_hotspots_sp, add=T)
angNwat <- over(angling_hotspots_sp, NHD_WA_buff)
# add lake ID onto angling events
angling_hotspots_wat <- angling_hotspots_sp@data
angling_hotspots_wat$PermID <- angNwat$Permanent_
angling_hotspots_wat$latitude_cent <- angNwat$latitude_cent
angling_hotspots_wat$longitude_cent <- angNwat$longitude_cent

## Cleanup and Export
# Identified hotspots with duplicate bobber ID, lake, and date combinations to get trips
angling_hotspots_wat$bdoID <- paste(angling_hotspots_wat$bobber_id, angling_hotspots_wat$OBJECTID, angling_hotspots_wat$date, sep='x')
angling_hotspots_wat$uniq <- !duplicated(angling_hotspots_wat$bdoID)

# Table for first round of analyses
toa <- angling_hotspots_wat[angling_hotspots_wat$uniq==T,c("bobber_id","date","PermID","latitude_cent","longitude_cent")]

# Write table to analyse
write.csv(toa, "./analysis/anglingXwater.csv", row.names=F)
