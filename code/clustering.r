###
### Code for K-Means clustering of lakes
###

# Read trip matrix
aud <- read.csv("./analysis/anglingXwater.csv", header=TRUE, na.strings=NULL, stringsAsFactors=F)

# Reformat and delete missing values
aud$date <- as.Date(aud$date)
aud$OBJECTID <- as.numeric(aud$OBJECTID)
aud_clean <- na.omit(aud)

# Standardize the data
aud_scaled <- scale(aud_clean)

# Determine number of clusters


# K-Means Cluster Analysis
aud_k <- kmeans(aud_scaled[,3], centers = 10, nstart = 25)

# Append cluster assignment
