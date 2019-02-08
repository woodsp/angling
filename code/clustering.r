
###
### K-Means clustering of lakes
### run from top-level directory of angling repository
###

install.packages("vegan")
install.packages("simba")
install.packages("cluster")
install.packages("ecodist")
install.packages("gclus")
install.packages("pastecs")
install.packages("NbClust")
install.packages("clusteval")
install.packages("MASS")
library("vegan")
library("simba")
library("cluster")
library("ecodist")
library("gclus")
library("pastecs")
library("NbClust")
library("clusteval")
library("MASS")

# ----------------------------------------------------------------------------
# Read data and variable screening
angler <- read.csv("./analysis/anglingXwater.csv", header=TRUE, na.strings=NULL, stringsAsFactors=F)

# remove first column containing user names
angler[1]<-NULL
str(angler)
dim(angler)

# abbreviate names (not used)
#temp_names<-abbreviate(row.names(angler))

# ----------------------------------------------------------------------------
# Multivariate Resemblance 

# CALCULATING COEFFICIENTS OF SIMILARITY FOR LAKES ACCORDING TO USER VISITATION
angler.jac <- sim(angler, method = "jaccard")

# ---------------------------------------------------------------------------
# Hierarchical Cluster Analysis 

# Average linkage (UPGMA)
angler.jacd<-vegdist(angler,method='jaccard')
anglercl.ave<-hclust(angler.jacd,method='average')
plot(anglercl.ave) 

# clustering performance
coef.hclust(anglercl.ave) 
hclus.cophenetic(angler.jacd,anglercl.ave)

# scree plot
hclus.scree(anglercl.ave)

# Ward's Method
anglercl.ward<-hclust(angler.jacd,method='ward.D2')
plot(anglercl.ward) 
plot(anglercl.ave,main='Average-linkage Dendrogram',xlab='Anglers',ylab='Jaccard Dissimilar')

# clustering performance

coef.hclust(anglercl.ward) 
hclus.cophenetic(angler.jacd,anglercl.ward)

# scree plot
hclus.scree(anglercl.ward)

# ------------------------------------------------------------------------
# Non-hierarchical Cluster Analysis 

nhclus.scree(angler.jacd,max.k=30)

anglercl.kmeans<-kmeans(angler.jacd,centers=10,iter.max=10000,nstart=25)
anglercl.kmeans$cluster
anglercl.class<-anglercl.kmeans$cluster
names(anglercl.kmeans)

plot(silhouette(anglercl.kmeans$cluster,angler.jacd))

anglercl.kmeans.cas<-cascadeKM(angler.jacd,inf.gr=3,sup.gr=10,iter=100)
plot(anglercl.kmeans.cas,sortg=T)
anglercl.kmeans.cas$results