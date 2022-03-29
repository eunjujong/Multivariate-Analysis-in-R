data <- read.csv("food.csv")

rownames(data)=data[,1]
data=data[,-1]

summary(data)

## Step 1 -Calculate variances, covariances, correlations
#variances
var1=var(data$Bread)
var2=var(data$Hamburger)
var3=var(data$Butter)
var4=var(data$Apples)
var5=var(data$Tomato)

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

#Identity Matrix (5x5)
ident=diag(5)
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

y=as.matrix(data)%*%eigenvectors
#all PC values
y


## Step 4 - Check variance estimates of the pcs and all other properties
var1+var2+var3+var4+var5
percentvars

percentvars_pc=eigenvalues/sum(eigenvalues)
percentvars_pc

## C: sum of variances  
var1+var2+var3+var4+var5
var(y1)+var(y2)+var(y3)+var(y4)+var(y5)

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
var1+var2+var3+var4+var5
var(y1)+var(y2)+var(y3)+var(y4)+var(y5)

## Step 5 - Nice plots and Interpret PCs

ts.plot(cbind(percentvars,percentvars_pc),col=c("blue","red"),xlab="ith vector",ylab="percent variance") 
legend(4, .5, legend=c("X", "PC"), col=c("blue", "red"), lty=c(1,1), cex=.8)

plot(y1,y2, type="n", xlim=c(min(y1)-5, max(y1)+5), xlab=expression('y'[1]), ylab=expression('y'[2]))
text(y1,y2, labels = rownames(data), cex=.8, col=1:24)

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

res.pca <- prcomp(data, scale = FALSE)
fviz_eig(res.pca, addlabels = TRUE)

var <- get_pca_var(res.pca)
var

library("corrplot")
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


## Step 6 - regression, use as an input
set.seed(1002)
dv=rowSums(data)+rnorm(24,mean=0,sd=10)
summary(lm(dv~as.matrix(data)))

summary(lm(dv~as.matrix(y)))

## let's pick the best two
cor(dv,data)
summary(lm(dv~data$Hamburger+data$Tomato))
summary(lm(dv~y1+y2))

## Step 7 - standardize variables, see how PC changes.  "each is equally important to diet"
data=scale(data)
data=as.data.frame(data)

#variances
var1=var(data$Bread)
var2=var(data$Hamburger)
var3=var(data$Butter)
var4=var(data$Apples)
var5=var(data$Tomato)

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

#Identity Matrix (5x5)
ident=diag(5)
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

y=as.matrix(data)%*%eigenvectors
#all PC values
y

var1+var2+var3+var4+var5
percentvars

percentvars_pc=eigenvalues/sum(eigenvalues)
percentvars_pc

## C: sum of variances  
var1+var2+var3+var4+var5
var(y1)+var(y2)+var(y3)+var(y4)+var(y5)

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
legend(4, .5, legend=c("X", "PC"), col=c("blue", "red"), lty=c(1,1), cex=.8)

plot(y1,y2, type="n", xlim=c(min(y1)-3, max(y1)+3), xlab=expression('y'[1]), ylab=expression('y'[2]))
text(y1,y2, labels = rownames(data), cex=.8, col=1:24)

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

res.pca <- prcomp(data, scale = FALSE)
fviz_eig(res.pca, addlabels = TRUE)

var <- get_pca_var(res.pca)
var

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             select.ind = list(cos2 = 10)
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PCs
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
)


set.seed(1002)
dv=rowSums(data)+rnorm(24,mean=0,sd=1)
summary(lm(dv~as.matrix(data)))

summary(lm(dv~as.matrix(y)))

## let's pick the best two
cor(dv,data)
summary(lm(dv~data$Hamburger+data$Tomato))
summary(lm(dv~y1+y2))

