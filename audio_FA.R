data <- read.csv("audio.csv")
data

rownames(data)=data[,1]
data=data[,-1]

## Step 1 - Explore data

summary(data)

var1=var(data$L500)
var2=var(data$L1000)
var3=var(data$L2000)
var4=var(data$L4000)
var5=var(data$R500)
var6=var(data$R1000)
var7=var(data$R2000)
var8=var(data$R4000)

rho=cor(data)
rho

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
L=matrix(nrow=8,ncol=m)
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

phi=diag(8)*unique

recreate=L%*%t(L)+phi
recreate

rho


## Step 6 - Create Residual Matrix

residual=rho-recreate
residual  ## check to see if off-diagonal elements are "small"

# If sum of square of unused eigenvalues is small, then the residual matrix is small:
sum(eigenvalues[4:8]^2)  ## sum of square of non-used eigenvalues

sum((rho-recreate)^2) / 2 # sum of squared Σ−(𝐿𝐿^′+𝜓) 
sum(residual[lower.tri(residual)]^2)  ## sum of square of off-diagonal elements

sum((rho-recreate)^2) / 2 <= sum(eigenvalues[4:8]^2)


## Step 7  - Plot pairs of loadings to interpret factor loadings
## if we can't tell, we may need to do a varimax rotation

plot(L[,1],L[,2],col=1:8 ,xlab="Loading 1",ylab="Loading 2", ylim=c(min(L[,2])-.2, max(L[,2])+.2))
text(L[,1],L[,2],names(data), pos=3, cex=.5)
abline(h=0, lty=2)


## Step 8

install.packages('psych')
library(psych)

## Factors 1 and 2:
## original plot
fit2 <- principal(data, nfactors=2, rotate="none")
fit2

fit <- principal(data, nfactors=2, rotate="varimax")
fit
plot(fit$loadings[,1],fit$loadings[,2],col=1:8, xlab="Loading 1",ylab="Loading 2", ylim=c(min(fit$loadings[,2])-.1, max(fit$loadings[,2])+.1))
text(fit$loadings[,1],fit$loadings[,2],names(data), pos=1, cex=.5)


## Plot factor loadings of factor 1 and 3
plot(L[,1],L[,3],col=1:8 ,xlab="Loading 1",ylab="Loading 3", ylim=c(min(L[,3])-.2, max(L[,3])+.2))
text(L[,1],L[,3],names(data), pos=3, cex=.5)
abline(h=0, lty=2)


## Factors 1 and 3:

## original plot
fit2 <- principal(data, nfactors=3, rotate="none")
fit2

fit <- principal(data, nfactors=3, rotate="varimax")
fit
plot(fit$loadings[,1],fit$loadings[,3],col=1:8, xlab="Loading 1",ylab="Loading 3", ylim=c(min(fit$loadings[,3])-.1, max(fit$loadings[,3])+.1))
text(fit$loadings[,1],fit$loadings[,3],names(data), pos=1, cex=.5)
