# Install packages
# install.packages("lubridate")
# install.packages("plyr")
# install.packages("data.table")
library(plyr)
library(lubridate)
library(data.table)

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
                               "source_permid", "longitude_cent", "latitude_cent", "state"))

# Duplicate source date and waterbody variables for destination
aud.multitrip$dest_date <- aud.multitrip$source_date
aud.multitrip$dest_permid <- aud.multitrip$source_permid

# Get source and destination waterbody info in matrix
movement <- data.frame("bobber_id" = character(), "source_date" = character(), 
                       "source_permid" = character(), "dest_date" = character(), 
                       "dest_permid" = character(), "Index" = numeric())
for (i in 1:(nrow(aud.multitrip)-1))
{
  if ((aud.multitrip$bobber_id[i]==aud.multitrip$bobber_id[i+1]) & (aud.multitrip$source_permid[i]!=aud.multitrip$source_permid[i+1]))
  {
    source <- aud.multitrip[i, c("bobber_id", "source_date", "source_permid")]
    dest <- aud.multitrip[i+1, c("dest_date", "dest_permid")]
    travel <- cbind(source, dest)
    movement <- rbind(movement, travel)
  }
}

# Write table to analyze
write.csv(movement, "./analysis/sourceXdest.csv", row.names=F)

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
