
## Set working dir
setwd("~/Desktop/STAT 630/HW")

## Read in Data
data <- read.csv("food.csv")
data

rownames(data)=data[,1]
data=data[,-1]
data=scale(data)  ## standardize data
data

## Explore the data
summary(data)
rho=cor(data)
rho

# FALSE if the requested package is not found
require(graphics)
#library(graphics)


## Single Linkage

## Step 1 - default: euclidean distance
D=dist(data)
D

res.dist <- get_dist(data, stand = TRUE, method = "pearson")

fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Step 2 - Run hclust()
hc<-hclust(D,"single")
summary(hc)

# Step 3 - Plot dendrogram
plot(hc, xlab="Clusters",ylab="Levels")


# Step 4 - choose number of clusters i
i = 8
memb<-cutree(hc,k=i)
memb

# install.packages("dendextend")
library(dendextend)

# Plot dendrogram based on the number of clusters
dend <- as.dendrogram(hc)
dend <- color_branches(dend, k =8)
plot(dend, main = "Colored branches")


# Step 5 - get clustser centers
cent<-NULL
for (k in 1:i){cent<-rbind(cent,colMeans(data[memb==k,,drop=FALSE]))}
cent

## Step 6 - Calculate sum of total SS . within SS for each cluster

one=sum((data[memb==1,,drop=FALSE]-cent[1,])^2)
two=sum((data[memb==2,,drop=FALSE]-cent[2,])^2)
three=sum((data[memb==3,,drop=FALSE]-cent[3,])^2)
four=sum((data[memb==4,,drop=FALSE]-cent[4,])^2)
five=sum((data[memb==5,,drop=FALSE]-cent[5,])^2)
six=sum((data[memb==6,,drop=FALSE]-cent[6,])^2)
sev=sum((data[memb==7,,drop=FALSE]-cent[7,])^2)
eig=sum((data[memb==8,,drop=FALSE]-cent[8,])^2)
one
two
three
four
tss_single=one+two+three+four  ## total sum of squares from cluster centroid
tss_single
one+two+three+four+five+six+sev+eig

## kmeans clustering
library(flexclust)


# Step 1 - Run kmeans and analyze clusters
i=3
cl=kmeans(data,i)  ## let's keep same number of clusters as before
cl

cl$betweenss
cl$totss

100*cl$betweenss/cl$totss

# Step 2 - Plot clusters
plot(data, col = cl$cluster)

plot(data$Hamburger, data$Bread, xlab="Hamburger", ylab="Bread", col = cl$cluster)
text(data$Hamburger, data$Bread, labels = rownames(data), cex=.5, col=cl$cluster)

plot(data$Butter, data$Hamburger, xlab="Butter", ylab="Hamburger", col = cl$cluster)
text(data$Butter, data$Hamburger, labels = rownames(data), cex=.5, col=cl$cluster)

plot(data$Butter, data$Bread, xlab="Butter", ylab="Bread", col = cl$cluster)
text(data$Butter, data$Bread, labels = rownames(data), cex=.5, col=cl$cluster)

plot(data$Apples, data$Bread, xlab="Apples", ylab="Bread", col = cl$cluster)
text(data$Apples, data$Bread, labels = rownames(data), cex=.5, col=cl$cluster)

plot(data$Apples, data$Hamburger, xlab="Apples", ylab="Hamburger", col = cl$cluster)
text(data$Apples, data$Hamburger, labels = rownames(data), cex=.5, col=cl$cluster)

plot(data$Apples, data$Butter, xlab="Apples", ylab="Butter", col = cl$cluster)
text(data$Apples, data$Butter, labels = rownames(data), cex=.5, col=cl$cluster)

plot(data$Tomato, data$Bread, xlab="Tomato", ylab="Bread", col = cl$cluster)
text(data$Tomato, data$Bread, labels = rownames(data), cex=.5, col=cl$cluster)

plot(data$Tomato, data$Hamburger, xlab="Tomato", ylab="Hamburger", col = cl$cluster)
text(data$Tomato, data$Hamburger, labels = rownames(data), cex=.5, col=cl$cluster)

plot(data$Tomato, data$Butter, xlab="Tomato", ylab="Butter", col = cl$cluster)
text(data$Tomato, data$Butter, labels = rownames(data), cex=.5, col=cl$cluster)

plot(data$Tomato, data$Apples, xlab="Tomato", ylab="Apples", col = cl$cluster)
text(data$Tomato, data$Apples, labels = rownames(data), cex=.5, col=cl$cluster)

points(cl$centers[1,], col=cl$cluster, pch = 20)
text(data,rownames(data),col=cl$cluster)



# Step 3 - Choose k  (plot total sum of squares)
tss<-rep(0,8)
for (k in 1:8){tss[k]=kmeans(data,k)$tot.withinss}
plot(1:8,tss, xlab="# of clusters", ylab="Total Within Sum of Squares")

## you want to see where it no longer decreases much (maybe 4 or 5 clusters), notice single linkage
## fits worse (higher tss) than all except 1 cluster.  it even does worse than 2!

# Step 4 - Interpret clusters
cl$centers

cl$cluster


# Step 5  - Plot clusters with pca (this makes your intreptation even better)

## here we are putting food names to the clusters...it helps people understand.
install.packages("pca3d")
install.packages("pca2d")

library(pca3d)
library(pca2d)

?pca3d
pca <- prcomp(data, scale.= TRUE )
pca2d( pca, group=cl$cluster )
pca2d( pca, group=cl$cluster,show.labels=TRUE)
pca3d( pca, group=cl$cluster,show.labels=TRUE )


## Instead of pca above

res.pca <- prcomp(data, scale = TRUE)
fviz_eig(res.pca)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


## Another way of K-Means

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
k2=kmeans(data, centers = i, nstart = 25)
str(k2)
k2
fviz_cluster(k2, data = data)
distance=get_dist(data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

set.seed(123)

# function to compute total within-cluster sum of square 
wss=function(k) {
  kmeans(data, k, nstart = 10 )$tot.withinss
}

wss

set.seed(123)

fviz_nbclust(data, kmeans, method = "wss") + 
  geom_vline(xintercept=3, linetype="dashed", color = "red") +
  labs(subtitle = "Elbow Method")
fviz_nbclust(data, kmeans, method = "silhouette")

gap_stat=clusGap(data, FUN = kmeans, nstart = 25,
                    K.max = 8, B = 50)
fviz_gap_stat(gap_stat)

install.packages("NbClust",dependencies = TRUE)
library(NbClust)
nb <- NbClust(data, diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=5, method = "kmeans", 
              index = "all", alphaBeale = 0.1)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

km <- kmeans(data, centers = 3, nstart = 25)
#plot results of final k-means model
fviz_cluster(km, data = data)
