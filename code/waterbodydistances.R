# Open packages
library(plyr)
library(lubridate)
library(data.table)
library(geosphere)

# Read in anglingXwater
aud <- read.csv("./analysis/anglingXwater_usa.csv", header = TRUE, colClasses = c("character",                                                                                   
                                "character", "character", "numeric", "numeric", "character"))
mdy(aud$date)

# Select PNW waterbodies
pnw <- c("WA", "OR", "ID")
pnw.aud <- aud[aud$state==pnw, ]
pnw.lakes <- pnw.aud[!duplicated(pnw.aud$PERMANENT_IDENTIFIER), ]
pnw.lakes <- pnw.lakes[, c("PERMANENT_IDENTIFIER", "longitude_cent", "latitude_cent", "state")]

# Find unique lake cobinations and merge location data into new df
pnw.lake2lake <- combn(pnw.lakes$PERMANENT_IDENTIFIER, 2, simplify = FALSE)
pnw <- data.frame(matrix(unlist(pnw.lake2lake), nrow = length(pnw.lake2lake), byrow = T))
names(pnw) <- c("lakeone", "laketwo")
pnw$lakeone <- as.character(pnw$lakeone)
pnw$laketwo <- as.character(pnw$laketwo)
pnw.distance <- merge(pnw, pnw.lakes, by.x = 'lakeone', by.y = 'PERMANENT_IDENTIFIER')
setnames(pnw.distance, old = c("longitude_cent", "latitude_cent", "state"), 
        new = c("lakeone_longitude_cent", "lakeone_latitude_cent", "lakeone_state"))
pnw.lakedistance <- merge(pnw.distance, pnw.lakes, by.x = 'laketwo', by.y = 'PERMANENT_IDENTIFIER')
setnames(pnw.lakedistance, old = c("longitude_cent", "latitude_cent", "state"), 
         new = c("laketwo_longitude_cent", "laketwo_latitude_cent", "laketwo_state"))

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
                 "point=", pnw.lakedistance$lakeone_latitude_cent[i], ",", pnw.lakedistance$lakeone_longitude_cent[i],
                 "&point=", pnw.lakedistance$laketwo_latitude_cent[i], ",", pnw.lakedistance$laketwo_longitude_cent[i],
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
  lakeonepts <- ghdtout[i, c("lakeone_longitude_cent", "lakeone_latitude_cent")]
  laketwopts <- ghdtout[i, c("laketwo_longitude_cent", "laketwo_latitude_cent")]
  ghdtout[i, "distance"] <- distGeo(lakeonepts, laketwopts)
}

# Write table to analyze
write.csv(ghdtout, "./analysis/pnwlakeXlakedistance.csv", row.names=F)