###
### Code for K-Means clustering of lakes
###

# Read trip matrix
aud <- read.csv("./analysis/anglingXwater.csv", header=TRUE, na.strings=NULL, stringsAsFactors=F)

# Reformat and delete missing values
aud$date <- as.Date(aud$date)
aud$Permanent_ <- as.numeric(aud$Permanent_)
aud_clean <- na.omit(aud)

# Determine number of clusters using elbow method
library(klaR)

# Compute and plot within diff for k = 2 to k = 20.
k.max <- 20
wss <- sapply(1:k.max, 
              function(k){set.seed(100000)
                sum(kmodes(aud_clean, k, iter.max = 100 ,weighted = FALSE)$withindiff)})
wss

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# K-Modes Cluster Analysis
cluster.results <-kmodes(aud_clean, 6 ,iter.max = 100, weighted = FALSE ) 
print(cluster.results)

# Append cluster assignment
kmclust <- factor(cluster.results$cluster)
aud_clean$cluster <- kmclust
