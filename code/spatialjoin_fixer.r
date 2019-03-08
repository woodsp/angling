###
### Clip the current anglingXwater.csv events to ones from continental US
###   someday this should happen in spatialjoin.r.  see issue #5.
###   for now, we'll just export a US-clipped version of anglingXwater for further analyses
###

library(rgdal)
# library(raster)
# library(rgeos)
library(sf)

## Import angling x water data (output of spatialjoin.r)
##   and create spatial data frame
axw_raw <- read.csv("analysis/anglingXwater.csv")
axw <- SpatialPointsDataFrame(axw_raw[ ,c("longitude_cent","latitude_cent")], axw_raw, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


## Import US border shape
##   representing the continental US plus the great lakes
##   created by merging and dissolving the US census border with border of great lakes
usa <- readOGR("data", "cb_2017_us_state_20m_N_hydro_p_diss")
usa_wgs <- spTransform(usa, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

## Join angling events to continental USA shape
angusa <- over(axw, usa_wgs)

## Drop records that aren't in USA
axw_usa <- axw_raw[!is.na(angusa), ]

## Write an updated anglingXwater for events in USA (and great lakes)
write.csv(axw_usa, "./analysis/anglingXwater_usa.csv", row.names=F)

## QAQC
plot(usa_wgs)
plot(axw, add=T)
plot(axw[which(is.na(angusa)), ], add=T, col="red")

