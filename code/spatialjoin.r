###
### Code to intersect angling trips with lake polygons
###

## Inputs are angling events and polygons of lakes
## Outputs are table of angling events by lake

# Read angling point events
angling_hotspots <- read.csv("/Users/rachelfricke/Desktop/Olden_Lab/Angler_Behavior/IB_Data_Mar18.csv", header = TRUE, na.strings = NULL)

# Split timestamp column into date and time, delete extraneous variables
library(tidyr)
angling_hotspots <- separate(angling_hotspots, "timestamp", c("date", "time"), " ")
angling_hotspots <- angling_hotspots[,-c(5:20,23)]

# Subset 2017 records
angling_hotspots <- angling_hotspots[which((angling_hotspots$date > "2016-12-31") & (angling_hotspots$date < "2018-01-01")), names(angling_hotspots) %in% c("hotspot_id", "latitude", "longitude", "bobber_id", "date", "time")]

# Read NHD spatial data
library(rgdal)
NHD_WA <- readOGR(dsn="/Users/rachelfricke/Desktop/Olden_Lab/Angler_Behavior/NHD_WA", layer="NHDWaterbody", stringsAsFactors = )

# Filter NHD to lakes
NHD_WA <- NHD_WA[which((NHD_WA@data$FCode >= "39000") & (NHD_WA@data$FCode <= "46599")), names(NHD_WA@data) %in% c("OBJECTID", "Permanent_", "FDate", "Resolution", "GNIS_ID", "GNIS_Name", "AreaSqKm", "Elevation", "ReachCode", "FType", "FCode", "Visibility", "Shape_Leng", "Shape_Area")]

# Buffer NHD lakes by 50m
library(raster)
library(rgeos)
buffer(NHD_WA, width = 50)

# Intersect angling points with buffered lake polys


# Remove hotspots with duplicate bobber ID, lake, and date combinations to get trips



