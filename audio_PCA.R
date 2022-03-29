data <- read.csv("audio.csv")

rownames(data)=data[,1]
data=data[,-1]

summary(data)

## Step 1 -Calculate variances, covariances, correlations
#variances
var1=var(data$L500)
var2=var(data$L1000)
var3=var(data$L2000)
var4=var(data$L4000)
var5=var(data$R500)
var6=var(data$R1000)
var7=var(data$R2000)
var8=var(data$R4000)

#correlation matrix
rho=cor(data)
rho

## Step 2 - Define the problem in terms of principal components
sigma=var(data)
sigma

#SD
sd=sqrt(sigma)
sd

vars=diag(sigma)
vars
percentvars=vars/sum(vars)
percentvars

#Identity Matrix (8x8)
ident=diag(8)
ident

## Step 3 - Compute all the eigenvalues and eigenvectors in R
eigenvalues=eigen(sigma)$values
eigenvectors=eigen(sigma)$vectors

#eigenvalues -> importance of the variables
eigenvalues
#eigenvectors -> we want to look at the ones with high magnitudes 
#(-): inverse relationship between the price of a product and the value of a specific PC
eigenvectors

# define principal componenets
y1=as.matrix(data)%*%(eigenvectors[,1])
y2=as.matrix(data)%*%(eigenvectors[,2])
y3=as.matrix(data)%*%(eigenvectors[,3])
y4=as.matrix(data)%*%(eigenvectors[,4])
y5=as.matrix(data)%*%(eigenvectors[,5])
y6=as.matrix(data)%*%(eigenvectors[,6])
y7=as.matrix(data)%*%(eigenvectors[,7])
y8=as.matrix(data)%*%(eigenvectors[,8])

y=as.matrix(data)%*%eigenvectors
#all PC values
y


## Step 4 - Check variance estimates of the pcs and all other properties
var1+var2+var3+var4+var5+var6+var7+var8
percentvars

percentvars_pc=eigenvalues/sum(eigenvalues)
percentvars_pc

## C: sum of variances  
var1+var2+var3+var4+var5+var6+var7+var8
var(y1)+var(y2)+var(y3)+var(y4)+var(y5)+var(y6)+var(y7)+var(y8)

## D: Magnitude of eigenvectors are importance of kth variable in the ith PC
eigenvectors

## E:  correlation between Yi and Xk
eigenvectors[,1]
cor(y1,data)
eigenvectors[,1]*sqrt(eigenvalues[1])/sqrt(diag(vars))


# E: are they uncorrelated?
sigma_pc=var(y)
sigma_pc
rho_pc=cor(y)
rho_pc

## F: sum(variances of variables) == sum(variances of PCs)
var1+var2+var3+var4+var5+var6+var7+var8
var(y1)+var(y2)+var(y3)+var(y4)+var(y5)+var(y6)+var(y7)+var(y8)

## Step 5 - Nice plots and Interpret PCs

ts.plot(cbind(percentvars,percentvars_pc),col=c("blue","red"),xlab="ith vector",ylab="percent variance") 
legend(6.5, .55, legend=c("X", "PC"), col=c("blue", "red"), lty=c(1,1), cex=.8)

plot(y1,y2, type="n", xlim=c(min(y1)-5, max(y1)+5), xlab=expression('y'[1]), ylab=expression('y'[2]))
text(y1,y2, labels = rownames(data), cex=.8, col=1:100)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
pc <- prcomp(data,
             center = TRUE,
             scale. = FALSE)
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

install.packages('psych')
library(psych)
library(FactoMineR)
library("factoextra")
library("corrplot")
res.pca <- prcomp(data, scale = FALSE)
fviz_eig(res.pca, addlabels = TRUE)
var <- get_pca_var(res.pca)
corrplot(var$cos2, is.corr=FALSE)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PCs
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
)

# Contributions of variables to PC1&PC2
fviz_contrib(res.pca, choice = "var", axes = 1, top = 5, fill="blue", col="blue")
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 5, fill="orange", col="orange")

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:5, fill="blue", col="blue")

## individuals contributing the most to the PCs
fviz_pca_ind(res.pca, select.ind = list(contrib = 5))

## Step 6 - regression, use as an input
set.seed(1002)
dv=rowSums(data)+rnorm(100,mean=0,sd=10)
summary(lm(dv~as.matrix(data)))

summary(lm(dv~as.matrix(y)))

## let's pick the best four
cor(dv,data)
summary(lm(dv~data$L4000+data$L2000+data$R4000+data$R2000))
summary(lm(dv~y1+y2+y3+y4))

## Step 7 - standardize variables, see how PC changes.  "each is equally important to diet"
data=scale(data)
data=as.data.frame(data)

#variances
var1=var(data$L500)
var2=var(data$L1000)
var3=var(data$L2000)
var4=var(data$L4000)
var5=var(data$R500)
var6=var(data$R1000)
var7=var(data$R2000)
var8=var(data$R4000)

#correlation matrix
rho=cor(data)
rho

sigma=var(data)
sigma

#SD
sd=sqrt(sigma)
sd

vars=diag(sigma)
vars
percentvars=vars/sum(vars)
percentvars

#Identity Matrix (8x8)
ident=diag(8)
ident

eigenvalues=eigen(sigma)$values
eigenvectors=eigen(sigma)$vectors

eigenvalues
eigenvectors

# define principal componenets
y1=as.matrix(data)%*%(eigenvectors[,1])
y2=as.matrix(data)%*%(eigenvectors[,2])
y3=as.matrix(data)%*%(eigenvectors[,3])
y4=as.matrix(data)%*%(eigenvectors[,4])
y5=as.matrix(data)%*%(eigenvectors[,5])
y6=as.matrix(data)%*%(eigenvectors[,6])
y7=as.matrix(data)%*%(eigenvectors[,7])
y8=as.matrix(data)%*%(eigenvectors[,8])

y=as.matrix(data)%*%eigenvectors
#all PC values
y

var1+var2+var3+var4+var5+var6+var7+var8
percentvars

percentvars_pc=eigenvalues/sum(eigenvalues)
percentvars_pc

## C: sum of variances  
var1+var2+var3+var4+var5+var6+var7+var8
var(y1)+var(y2)+var(y3)+var(y4)+var(y5)+var(y6)+var(y7)+var(y8)

## D: Magnitude of eigenvectors are importance of kth variable in the ith PC
eigenvectors

## E:  correlation between Yi and Xk
eigenvectors[,1]
cor(y1,data)
eigenvectors[,1]*sqrt(eigenvalues[1])/sqrt(diag(vars))


# E: are they uncorrelated?
sigma_pc=var(y)
sigma_pc
rho_pc=cor(y)
rho_pc

ts.plot(cbind(percentvars,percentvars_pc),col=c("blue","red"),xlab="ith vector",ylab="percent variance") 
legend(6.5, .5, legend=c("X", "PC"), col=c("blue", "red"), lty=c(1,1), cex=.8)

plot(y1,y2, type="n", xlim=c(min(y1)-5, max(y1)+5), xlab=expression('y'[1]), ylab=expression('y'[2]))
text(y1,y2, labels = rownames(data), cex=.8, col=1:100)

pc <- prcomp(data,
             center = TRUE,
             scale. = TRUE)
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

res.pca <- prcomp(data, scale = TRUE)
fviz_eig(res.pca, addlabels = TRUE)
var <- get_pca_var(res.pca)
corrplot(var$cos2, is.corr=FALSE)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PCs
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
)

# Contributions of variables to PC1&PC2
fviz_contrib(res.pca, choice = "var", axes = 1, top = 5, fill="blue", col="blue")
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 5, fill="orange", col="orange")

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:5, fill="blue", col="blue")

## individuals contributing the most to the PCs
fviz_pca_ind(res.pca, select.ind = list(contrib = 5))
data[c(67,71,55,40,78),]

set.seed(1002)
dv=rowSums(data)+rnorm(100,mean=0,sd=1)
summary(lm(dv~as.matrix(data)))

summary(lm(dv~as.matrix(y)))

## let's pick the best four
cor(dv,data)
summary(lm(dv~data$L1000+data$R1000+data$L500+data$L2000))
summary(lm(dv~y1+y2+y3+y4))

