
###
### K-Means clustering of lakes
###   run from top-level directory of angling repository
###

# Read trip matrix
aud <- read.csv("./analysis/anglingXwater.csv", header=TRUE, na.strings=NULL, stringsAsFactors=F)

# Reformat and delete missing values
aud$date <- as.Date(aud$date)
aud$PermID <- as.numeric(aud$PermID)
aud_clean <- na.omit(aud)

# Create table of users (rows) by lakes (cols)
#  with number of visits as cell values
library(reshape2)
aud_clean_wide <- dcast(aud_clean, bobber_id ~ PermID, value.var="PermID", length)


# Determine number of clusters using elbow method
library(klaR)

# Compute and plot within diff for k=2 to k=20.
k.max <- 20
wss <- sapply(1:k.max, function(k) {
                set.seed(100000)
                sum(kmodes(aud_clean, k, iter.max=100, weighted=FALSE)$withindiff)
              })

plot(1:k.max, wss, type="b", pch=19, frame=FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")

# K-Modes Cluster Analysis
cluster.results <-kmodes(aud_clean, 6 ,iter.max=100, weighted=FALSE) 
print(cluster.results)

# Append cluster assignment
aud_clean$cluster <- factor(cluster.results$cluster)



