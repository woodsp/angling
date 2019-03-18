# Install packages
# install.packages("lubridate")
# install.packages("plyr")
# install.packages("data.table")
library(plyr)
library(lubridate)
library(data.table)
library(geosphere)

# Read in anglingXwater
aud <- read.csv("./analysis/anglingXwater_usa.csv", header = TRUE, colClasses = c("character", 
                  "character", "character", "numeric", "numeric", "character"))
mdy(aud$date)

# Omit bobbers with only one trip
aud.multitrip <- aud[aud$bobber_id %in% aud$bobber_id[duplicated(aud$bobber_id)],]

# Sort by bobber_id then date
aud.multitrip <- arrange(aud.multitrip, bobber_id, date)

# Rename columns in source df
setnames(aud.multitrip, old = c("bobber_id", "date", "PERMANENT_IDENTIFIER", "longitude_cent", 
                               "latitude_cent", "state"), new = c("bobber_id", "source_date", 
                               "source_permid", "source_longitude_cent", "source_latitude_cent", "source_state"))

# Duplicate source date and waterbody variables for destination
aud.multitrip$dest_date <- aud.multitrip$source_date
aud.multitrip$dest_permid <- aud.multitrip$source_permid
aud.multitrip$dest_longitude_cent <- aud.multitrip$source_longitude_cent
aud.multitrip$dest_latitude_cent <- aud.multitrip$source_latitude_cent
aud.multitrip$dest_state <- aud.multitrip$source_state

# Get source and destination waterbody info in matrix
movement <- data.frame("bobber_id" = character(), "source_date" = character(), 
                       "source_permid" = character(), "source_longitude_cent" = numeric(), 
                       "source_latitude_cent" = numeric(), "source_state" = character(), 
                       "dest_date" = character(), "dest_permid" = character(), 
                       "dest_longitude_cent" = numeric(), "dest_latitude_cent" = numeric(), 
                       "dest_state" = character())
for (i in 1:(nrow(aud.multitrip)-1))
{
  if ((aud.multitrip$bobber_id[i]==aud.multitrip$bobber_id[i+1]) & (aud.multitrip$source_permid[i]!=aud.multitrip$source_permid[i+1]))
  {
    source <- aud.multitrip[i, c("bobber_id", "source_date", "source_permid", 
                                 "source_longitude_cent", "source_latitude_cent", "source_state")]
    dest <- aud.multitrip[i+1, c("dest_date", "dest_permid", "dest_longitude_cent", 
                                 "dest_latitude_cent", "dest_state")]
    travel <- cbind(source, dest)
    movement <- rbind(movement, travel)
  }
}

# Get road travel distance between source and destination
library(jsonlite)
# fxn to query every location
ghtry <- function(i, daurl) {
  tryCatch(fromJSON(daurl)$paths,
           error=function(e) { 
             data.frame(distance=NA,time=NA,weight=NA)
           })
}
ghops <- list()

for (i in 1:nrow(movement)) {
  daurl <- paste("http://vulpes.sefs.uw.edu/graphhopper/route?",
                 "point=", movement$source_latitude_cent[i], ",", movement$source_longitude_cent[i],
                 "&point=", movement$dest_latitude_cent[i], ",", movement$dest_longitude_cent[i],
                 "&instructions=false&calc_points=false", sep='')
  ghops[[i]] <- ghtry(i, daurl)[1:3]
  if (is.na(ghops[[i]]$distance)) { print(paste("graphhopper failed on user record",i,"of",nrow(movement))) }
}

## ## then reshape into a table
ghdt <- data.frame(matrix(unlist(ghops), ncol=3, byrow=T))
dimnames(ghdt)[[2]] <- names(ghops[[1]])
# ghdt$pid <- ths$pid
ghdtout <- cbind(movement, ghdt)

# Calculate Euclidean distance for ghop NA outputs
todo <- which(is.na(ghdtout$distance))
for (i in todo){
    sourcepts <- ghdtout[i, c("source_longitude_cent", "source_latitude_cent")]
    destpts <- ghdtout[i, c("dest_longitude_cent", "dest_latitude_cent")]
    ghdtout[i, "distance"] <- distGeo(sourcepts, destpts)
}

# Write table to analyze
write.csv(ghdtout, "./analysis/sourceXdest.csv", row.names=F)


# Old date calculation code
#start <- Sys.time()
#interval <- NA
#for(i in 1:(nrow(aud.multitrip)-1))
  #{
  #if(((aud.multitrip$bobber_id[i])==(aud.multitrip$bobber_id[i+1])) & ((aud.multitrip$PERMANENT_IDENTIFIER[i])!=(aud.multitrip$PERMANENT_IDENTIFIER[i+1])))
  #{
    #dateA <- as.Date(aud.multitrip$date[i+1], format = "%%%Y-%m-%d")
    #dateB <- as.Date(aud.multitrip$date[i], format = "%%%Y-%m-%d")
    #interval[i] <- as.numeric(difftime(dateA, dateB, units = c("days")))
  #} 
#}
#end <- Sys.time() ; end-start

# Make dataframe and rename
#days.between <- data.frame(matrix(unlist(interval, recursive = TRUE, use.names = TRUE)))
#days.between$matrix.unlist.interval..recursive...TRUE..use.names...TRUE..
#days.between <- na.omit(days.between)
#colnames(days.between)[colnames(days.between)=="matrix.unlist.interval..recursive...TRUE..use.names...TRUE.."] <- "interval"
