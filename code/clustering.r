
###
### K-Means clustering of lakes
### run from top-level directory of angling repository
###

library("vegan", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("simba", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("cluster", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("ecodist", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("gclus", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("pastecs", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("NbClust", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("clusteval", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("MASS", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

# ----------------------------------------------------------------------------
# Read data and variable screening
aud <- read.csv("./analysis/anglingXwater.csv", header=TRUE, na.strings=NULL, stringsAsFactors=F)

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