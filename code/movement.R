# Install packages
install.packages("lubridate")
install.packages("plyr")
library(plyr)
library(lubridate)

# Read in anglingXwater
aud <- read.csv("./analysis/anglingXwater_usa.csv", header = TRUE, colClasses = c("character", "character", "character", "numeric", "numeric", "character"))
aud$date <- as.Date(aud$date, "%m/%d/%Y")

# Omit bobbers with only one trip
aud.multitrip <- aud[aud$bobber_id %in% aud$bobber_id[duplicated(aud$bobber_id)],]

# Sort by bobber_id then date
aud.multitrip <- arrange(aud.multitrip, bobber_id, date)

# Loop through rows
start <- Sys.time()
interval <- NA
for(i in 1:(nrow(aud.multitrip)-1))
  {
  if(((aud.multitrip$bobber_id[i])==(aud.multitrip$bobber_id[i+1])) & ((aud.multitrip$PERMANENT_IDENTIFIER[i])!=(aud.multitrip$PERMANENT_IDENTIFIER[i+1])))
  {
    dateA <- as.Date(aud.multitrip$date[i+1], format = "%%%Y-%m-%d")
    dateB <- as.Date(aud.multitrip$date[i], format = "%%%Y-%m-%d")
    interval[i] <- as.numeric(difftime(dateA, dateB, units = c("days")))
  } 
}
end <- Sys.time() ; end-start

# Make dataframe and rename
days.between <- data.frame(matrix(unlist(interval, recursive = TRUE, use.names = TRUE)))
days.between$matrix.unlist.interval..recursive...TRUE..use.names...TRUE..
days.between <- na.omit(days.between)
colnames(days.between)[colnames(days.between)=="matrix.unlist.interval..recursive...TRUE..use.names...TRUE.."] <- "interval"
