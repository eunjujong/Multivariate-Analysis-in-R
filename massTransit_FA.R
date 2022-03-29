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

#remove nas
data=data[complete.cases(data),]

# convert numeric vars to dummies
install.packages("dummies")
library(dummies)
data$V41=as.factor(data$V41)
data$V42=as.factor(data$V42)
data$V43=as.factor(data$V43)
data$V47=as.factor(data$V47)
data$V48=as.factor(data$V48)
data$V52=as.factor(data$V52)
data=dummy.data.frame(data, sep = ".")
summary(data)

## Step 1 - Explore data
summary(data)

# Data standardization
#data=scale(data)
#data=as.data.frame(data)

# varList=list(1:length(data))
# for(i in 1:length(data)){
#   varList[i]=var(data[,i], na.rm=TRUE)
# }

# "complete.obs" enables to produce a correlation/variance matrix using non-missing entries. 
rho=cor(data)
rho
library(corrplot)
corrplot(rho, method = "circle", type="lower")

sigma=var(data)
sigma

vars=diag(sigma)
vars
percentvars=vars/sum(vars)
percentvars

## Step 2 - Compute the eigenvalues and eigenvactors of the correlation matrix
eigenvalues=eigen(rho)$values
eigenvalues
(eigenvalues)>1
m=3  ## can change this to include more factors
sum(eigenvalues[1:m])/length(eigenvalues) # Proportion of total variance due to m factors

eigenvectors=eigen(rho)$vectors
eigenvectors

## Step 3 - Compute Estimated Factor Loadings
L=matrix(nrow=length(data),ncol=m)
for (j in 1:m){
  L[,j]=sqrt(eigenvalues[j])*eigenvectors[,j]  
}

L  # first column is Factor 1, 2nd column is factor 2, 3rd column is factor 3

## Step 4 - Compute common variance and unique variance
round(L, 2)
common=rowSums(L^2)
unique=1-common  ## diagonal of error matrix

common
unique

data.frame(round(common, 2), round(unique, 2))

## Step 5 - Check the model to reproduce correlation

phi=diag(67)*unique

recreate=L%*%t(L)+phi
recreate


library(corrplot)
corrplot(recreate, method = "circle", type="lower")

## Step 6 - Create Residual Matrix

residual=rho-recreate
residual  ## check to see if off-diagonal elements are "small"

# If sum of square of unused eigenvalues is small, then the residual matrix is small:
sum(eigenvalues[(m+1):length(data)]^2)  ## sum of square of non-used eigenvalues

sum((rho-recreate)^2) / 2 # sum of squared Σ−(𝐿𝐿^′+𝜓) 
sum(residual[lower.tri(residual)]^2)  ## sum of square of off-diagonal elements

sum((rho-recreate)^2) / 2 <= sum(eigenvalues[4:8]^2)


## Step 7  - Plot pairs of loadings to interpret factor loadings
## if we can't tell, we may need to do a varimax rotation

plot(L[,2],L[,3],col=1:length(data) ,xlab="Loading 2",ylab="Loading 3")
text(L[,2],L[,3],names(data), pos=3, cex=.5)
abline(h=0, lty=2)


## Step 8

install.packages('psych')
library(psych)

## Factors 1 and 2:
## original plot
fit2 <- principal(data, nfactors=4, rotate="none")
fit2

fit <- principal(data, nfactors=4, rotate="varimax")
fit
plot(fit$loadings[,1],fit$loadings[,4],col=1:length(data), xlab="Loading 1",ylab="Loading 3")
text(fit$loadings[,1],fit$loadings[,4],names(data), pos=1, cex=.5)



## Plot factor loadings of factor 1 and 3
plot(L[,1],L[,3],col=1:length(data) ,xlab="Loading 1",ylab="Loading 3")
text(L[,1],L[,3],names(data), pos=3, cex=.5)
abline(h=0, lty=2)

## original plot
fit2 <- principal(data, nfactors=3, rotate="none")
fit2

fit <- principal(data, nfactors=3, rotate="varimax")
fit
plot(fit$loadings[,1],fit$loadings[,3],col=1:length(data), xlab="Loading 1",ylab="Loading 3")
text(fit$loadings[,1],fit$loadings[,3],names(data), pos=1, cex=.5)



## Plot factor loadings of factor 1 and 4
plot(L[,1],L[,4],col=1:length(data) ,xlab="Loading 1",ylab="Loading 4")
text(L[,1],L[,4],names(data), pos=3, cex=.5)
abline(h=0, lty=2)

## original plot
fit2 <- principal(data, nfactors=4, rotate="none")
fit2

fit <- principal(data, nfactors=4, rotate="varimax")
fit
plot(fit$loadings[,1],fit$loadings[,4],col=1:length(data), xlab="Loading 1",ylab="Loading 4")
text(fit$loadings[,1],fit$loadings[,4],names(data), pos=1, cex=.5)



## Plot factor loadings of factor 1 and 5
plot(L[,1],L[,5],col=1:length(data) ,xlab="Loading 1",ylab="Loading 5")
text(L[,1],L[,5],names(data), pos=3, cex=.5)
abline(h=0, lty=2)

## original plot
fit2 <- principal(data, nfactors=5, rotate="none")
fit2

fit <- principal(data, nfactors=5, rotate="varimax")
fit
plot(fit$loadings[,1],fit$loadings[,5],col=1:length(data), xlab="Loading 1",ylab="Loading 5")
text(fit$loadings[,1],fit$loadings[,5],names(data), pos=1, cex=.5)



## Plot factor loadings of factor 1 and 6
plot(L[,1],L[,6],col=1:length(data) ,xlab="Loading 1",ylab="Loading 6")
text(L[,1],L[,6],names(data), pos=3, cex=.5)
abline(h=0, lty=2)

## original plot
fit2 <- principal(data, nfactors=6, rotate="none")
fit2

fit <- principal(data, nfactors=6, rotate="varimax")
fit
plot(fit$loadings[,1],fit$loadings[,6],col=1:length(data), xlab="Loading 1",ylab="Loading 6")
text(fit$loadings[,1],fit$loadings[,6],names(data), pos=1, cex=.5)



## Plot factor loadings of factor 1 and 16
plot(L[,1],L[,16],col=1:length(data) ,xlab="Loading 1",ylab="Loading 16")
text(L[,1],L[,16],names(data), pos=3, cex=.5)
abline(h=0, lty=2)

## original plot
fit2 <- principal(data, nfactors=16, rotate="none")
fit2

fit <- principal(data, nfactors=16, rotate="varimax")
fit
plot(fit$loadings[,1],fit$loadings[,16],col=1:length(data), xlab="Loading 1",ylab="Loading 16")
text(fit$loadings[,1],fit$loadings[,16],names(data), pos=1, cex=.5)
