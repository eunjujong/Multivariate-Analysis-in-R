
## Set working dir
setwd("~/Desktop/STAT 630/HW")

data <- read.csv("masst.csv")
data

rownames(data)=data[,1]
data=data[,-1]

summary(data)

# Remove the last column
data <- data[,-(53:53)]

# convert to integer values
for(i in 1:length(data)){
  data[,i] <- suppressWarnings(as.numeric(data[,i]))
}

## # of rows with NA values
(which_nas <- apply(data, 1, function(X) any(is.na(X))))
length(which(which_nas))
length(which(which_nas))/nrow(data)

#remove nas
data=data[complete.cases(data),]

install.packages("dummies")
library(dummies)
data$V41=as.factor(data$V41)
data$V42=as.factor(data$V42)
data$V43=as.factor(data$V43)
data$V47=as.factor(data$V47)
data$V48=as.factor(data$V48)
data$V51=as.factor(data$V51)
data$V52=as.factor(data$V52)
data=dummy.data.frame(data, sep = ".")

## Explore the data
summary(data)

# Rescale data
data=scale(data)
summary(data)

rho=cor(data)
rho
library(corrplot)
corrplot(rho, method = "circle", type="lower")

# FALSE if the requested package is not found
require(graphics)
#library(graphics)

## Single Linkage

## Step 1 - default: euclidean distance
D=dist(data)
D

# Step 2 - Run hclust()
hc<-hclust(D,"single")
summary(hc)
hc2<-hclust(D,"complete")
summary(hc2)

# Step 3 - Plot dendrogram
plot(hc, xlab="Clusters",ylab="Levels")
plot(hc2, xlab="Clusters",ylab="Levels")

# Step 4 - choose number of clusters i
i = 7
memb<-cutree(hc,k=i)
memb

# install.packages("dendextend")
library(dendextend)

# Plot dendrogram based on the number of clusters
dend <- as.dendrogram(hc)
dend1 <- as.dendrogram(hc2)
dend <- color_branches(dend, k = i)
dend1 <- color_branches(dend1, k = i)
plot(dend, main = "Colored branches")
plot(dend1, main = "Colored branches")


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
seven=sum((data[memb==7,,drop=FALSE]-cent[7,])^2)

tss_single=one+two+three+four+five+six+seven  ## total sum of squares from cluster centroid
tss_single

## kmeans clustering
library(flexclust)

# Step 1 - Run kmeans and analyze clusters
cl=kmeans(data,i)  ## let's keep same number of clusters as before
cl
cl$cluster
cl$betweenss
cl$totalss

km2 <- kmeans(data, centers = 2, nstart = 25)
km2$betweenss / km2$totss
km3 <- kmeans(data, centers = 3, nstart = 25)
km3$betweenss / km3$totss
km4 <- kmeans(data, centers = 4, nstart = 25)
km4$betweenss / km4$totss
km5 <- kmeans(data, centers = 5, nstart = 25)
km5$betweenss / km5$totss
km6 <- kmeans(data, centers = 6, nstart = 25)
km6$betweenss / km6$totss
km7 <- kmeans(data, centers = 7, nstart = 25)
km7$betweenss / km7$totss

km <- kmeans(data, centers = 15, nstart = 25)
km$betweenss / km$totss

install.packages("vegan")
install.packages("klaR")
install.packages("caret")
library(klaR)
library(caret)
library (vegan)
library (cluster)
dis = dist(data)^2
res = kmeans(data,2)
sil = silhouette (res$cluster, dis)
windows() 
plot(sil)

# Step 2 - Plot clusters
plot(data, col = cl$cluster)

points(cl$centers[1,], col=cl$cluster, pch = 20)
text(data,rownames(data),col=cl$cluster)


## Enhanced k-means clustering
res.km <- eclust(data, "kmeans", nstart = 25)

# Step 3 - Choose k  (plot total sum of squares)
tss<-rep(0,6)
for (k in 1:6){tss[k]=kmeans(data,k)$tot.withinss}
plot(1:6,tss, xlab="# of clusters", ylab="Total Within Sum of Squares")


# Step 4 - Interpret clusters
cl$centers

cl$cluster


# Step 5  - Plot clusters with pca (this makes your intreptation even better)


install.packages("pca3d")
install.packages("pca2d")
library(pca3d)
library(pca2d)
?pca3d
pca <- prcomp(data, scale.= TRUE )
pca2d( pca, group=cl$cluster )
pca2d( pca, group=cl$cluster,show.labels=TRUE )
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

## make your statements in step 4 combine with step 5
## you may also want to do PCA to rename the axes. you should know how to do this!
# congratulations - very few people can explain PCA and clustering together. this will pay 
#         you dividends down the road in analytics.  Remember to B>0 (be positive!)

## Another way of K-Means

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
k=kmeans(data, centers = 6, nstart = 25)
str(k)
k
fviz_cluster(k, data = data)
distance=get_dist(data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

set.seed(123)

fviz_nbclust(data, kmeans, method = "wss") + 
  geom_vline(xintercept=2, linetype="dashed", color = "red") +
  labs(subtitle = "Elbow Method")
fviz_nbclust(data, kmeans, method = "silhouette")

fviz_nbclust(data, pam, method = "gap_stat")

gap_stat=clusGap(data, FUN = kmeans, nstart = 25,
                 K.max = 6, B = 50)
fviz_gap_stat(gap_stat)

km2 <- kmeans(data, centers = 2, nstart = 25)
fviz_cluster(km2, data = data)

km4 <- kmeans(data, centers = 4, nstart = 25)
fviz_cluster(km4, data = data)


data[c(143,241,269,21,40,447,3,275,587,91,341,514),]
