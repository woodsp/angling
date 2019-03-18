# Open packages
library(plyr)
library(lubridate)
library(data.table)
library(geosphere)

# Read in PNW iBobber and Flickr lakes
pnwlakes <- read.csv("./data/PNW_bobflick_lakes.csv", header = TRUE, 
                     colClasses = c("character", "numeric", "numeric", "character"))

# Remove duplicate waterbodies
pnwlakes <- pnwlakes[!duplicated(pnwlakes$Waterbody), ]
pnwlakes <- pnwlakes[, c("Waterbody", "Longitude", "Latitude")]

# Find unique lake cobinations and merge location data into new df
pnw.lake2lake <- combn(pnwlakes$Waterbody, 2, simplify = FALSE)
pnw <- data.frame(matrix(unlist(pnw.lake2lake), nrow = length(pnw.lake2lake), byrow = T))
names(pnw) <- c("lakeone", "laketwo")
pnw$lakeone <- as.character(pnw$lakeone)
pnw$laketwo <- as.character(pnw$laketwo)
pnw.distance <- merge(pnw, pnwlakes, by.x = 'lakeone', by.y = 'Waterbody')
setnames(pnw.distance, old = c("Longitude", "Latitude"), 
         new = c("lakeone_longitude", "lakeone_latitude"))
pnw.lakedistance <- merge(pnw.distance, pnwlakes, by.x = 'laketwo', by.y = 'Waterbody')
setnames(pnw.lakedistance, old = c("Longitude", "Latitude"), 
         new = c("laketwo_longitude", "laketwo_latitude"))

# Get road travel distance between lakes
library(jsonlite)
# fxn to query every location
ghtry <- function(i, daurl) {
  tryCatch(fromJSON(daurl)$paths,
           error=function(e) { 
             data.frame(distance=NA,time=NA,weight=NA)
           })
}
ghops <- list()

for (i in 1:nrow(pnw.lakedistance)) {
  daurl <- paste("http://vulpes.sefs.uw.edu/graphhopper/route?",
                 "point=", pnw.lakedistance$lakeone_latitude[i], ",", pnw.lakedistance$lakeone_longitude[i],
                 "&point=", pnw.lakedistance$laketwo_latitude[i], ",", pnw.lakedistance$laketwo_longitude[i],
                 "&instructions=false&calc_points=false", sep='')
  ghops[[i]] <- ghtry(i, daurl)[1:3]
  if (is.na(ghops[[i]]$distance)) { print(paste("graphhopper failed on user record",i,"of",nrow(pnw.lakedistance))) }
}

## ## then reshape into a table
ghdt <- data.frame(matrix(unlist(ghops), ncol=3, byrow=T))
dimnames(ghdt)[[2]] <- names(ghops[[1]])
# ghdt$pid <- ths$pid
ghdtout <- cbind(pnw.lakedistance, ghdt)

# Calculate Euclidean distance for ghop NA outputs
todo <- which(is.na(ghdtout$distance))
for (i in todo){
  lakeonepts <- ghdtout[i, c("lakeone_longitude", "lakeone_latitude")]
  laketwopts <- ghdtout[i, c("laketwo_longitude", "laketwo_latitude")]
  ghdtout[i, "distance"] <- distGeo(lakeonepts, laketwopts)
}

# Write table to analyze
write.csv(ghdtout, "./analysis/pnwlakeXlakedistance_ibobflick.csv", row.names=F)
