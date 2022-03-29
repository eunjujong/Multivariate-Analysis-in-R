## Step 0 - Read in Data

data <- read.csv("FinancialData.csv")
data=data[,2:3]
admired=c(rep(1,12),rep(2,12))

## Step 1 - Explore the data
summary(data)
plot(data$EBITASS,data$ROTC,col=admired)

## Step 2 - t.test
t.test(data$EBITASS[1:12],data$EBITASS[13:24])
t.test(data$ROTC[1:12],data$ROTC[13:24])

## Step 3  - summary statistics
pi1=which(admired==1)
pi2=which(admired==2)
xbar1=colMeans(data[pi1,])
xbar2=colMeans(data[pi2,])

S1=cov(data[pi1,])
S2=cov(data[pi2,])
Sp=(2*S1+2*S2)/4

plot(density(data[pi1,]$EBITASS),xlim=c(-.5,.5))
lines(density(data[pi2,]$EBITASS),col=2)


plot(density(data[pi1,]$ROTC),xlim=c(-.5,.5))
lines(density(data[pi2,]$ROTC),col=2)

## Step 4 - Calculate the linear combination and weights

y=(xbar1-xbar2)%*%solve(Sp)%*%t(as.matrix(data))
y=as.vector(y)
a=t((xbar1-xbar2)%*%solve(Sp))
astar=a/norm(a)
ystar=t(astar)%*%t(as.matrix(data))

plot(density(y[pi1]),xlim=c(-20,30))
lines(density(y[pi2]),col=2)

## Step 5 - Calculate cutoff and classify
cutoff=.5*(xbar1-xbar2)%*%solve(Sp)%*%(xbar1+xbar2)
cutoff=as.vector(cutoff)
classify=ifelse(y>cutoff,1,2)

abline(v=cutoff)

## Step 6 - Inspect Results and calculate separation
plot(y,rep(1,24),col=admired,ylab="")
abline(v=cutoff)

admired
classify

## Step 7 - Calculate upperbound and separation
upper=(xbar1-xbar2)%*%solve(Sp)%*%((xbar1-xbar2))

sy=(sum((y[pi1]-mean(y[pi1]))^2)+sum((y[pi2]-mean(y[pi2]))^2))/(length(pi1)+length(pi2)-2)
separation=abs(mean(y[pi1])-mean(y[pi2]))/sy

t(a)%*%Sp%*%(a)

## Step 8 - Calculate accuracy metrics
library(forecast)
c_accuracy(admired-1,classify-1)

library(ROCR)
pred<-prediction(-y,admired)
perf <- performance(pred,"tpr","fpr")
plot(perf)



### other , 
library(MASS)
model=lda(admired~-1+as.matrix(data))
summary(model)
predict(model)

## Fisher maximizes the difference between the means, normalized by a measure of the within class scatter




## Function Below
## make sure actuals and classifications are 0 (no) or 1 (yes) only 
##  Built by Matthew J. Schneider

c_accuracy=function(actuals,classifications){
  df=data.frame(actuals,classifications);
  
  
  tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
  fp=nrow(df[df$classifications==1 & df$actuals==0,]);
  fn=nrow(df[df$classifications==0 & df$actuals==1,]);
  tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
  
  
  recall=tp/(tp+fn)
  precision=tp/(tp+fp)
  accuracy=(tp+tn)/(tp+fn+fp+tn)
  tpr=recall
  fpr=fp/(fp+tn)
  fmeasure=2*precision*recall/(precision+recall)
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure")
  
  #print(scores)
  return(scores);
}



