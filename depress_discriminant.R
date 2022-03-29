## Step 0 - Read in Data

data <- read.csv("depres.csv")
summary(data)
data=data[,-c(1,2)]  ## remove obs and ID
data = data[,-c(28)] # omit CESD
depress=data$CASES

## Step 1 - Explore the data
library(ggplot2)
library(dplyr)

data$Depression=as.factor(data$CASES)
mu<-ddply(data, "Depression", summarise, grp.mean=mean(SEX))
p1<-ggplot(data, aes(x=data[,1], color=Depression)) +
  geom_density()+
  labs(x = "Sex", y = "Density" ) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Depression),
             linetype="dashed")

mu<-ddply(data, "Depression", summarise, grp.mean=mean(AGE))
p2<-ggplot(data, aes(x=data[,2], color=Depression)) +
  geom_density()+
  labs(x = "Age", y = "Density" ) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Depression),
             linetype="dashed")

mu<-ddply(data, "Depression", summarise, grp.mean=mean(MARITAL))
p3<-ggplot(data, aes(x=data[,3], color=Depression)) +
  geom_density()+
  labs(x = "Marital Status", y = "Density" ) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Depression),
             linetype="dashed")

mu<-ddply(data, "Depression", summarise, grp.mean=mean(EDUCAT))
p4<-ggplot(data, aes(x=data[,4], color=Depression)) +
  geom_density()+
  labs(x = "Education", y = "Density" ) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Depression),
             linetype="dashed")

mu<-ddply(data, "Depression", summarise, grp.mean=mean(EMPLOY))
p5<-ggplot(data, aes(x=data[,5], color=Depression)) +
  geom_density()+
  labs(x = "Employment Status", y = "Density" ) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Depression),
             linetype="dashed")

mu<-ddply(data, "Depression", summarise, grp.mean=mean(INCOME))
p6<-ggplot(data, aes(x=data[,6], color=Depression)) +
  geom_density()+
  labs(x = "Income", y = "Density" ) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Depression),
             linetype="dashed")

mu<-ddply(data, "Depression", summarise, grp.mean=mean(RELIG))
p7<-ggplot(data, aes(x=data[,7], color=Depression)) +
  geom_density()+
  labs(x = "Religion", y = "Density" ) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Depression),
             linetype="dashed")

mu<-ddply(data, "Depression", summarise, grp.mean=mean(DRINK))
p8<-ggplot(data, aes(x=data[,29], color=Depression)) +
  geom_density()+
  labs(x = "Drinking", y = "Density" ) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Depression),
             linetype="dashed")

mu<-ddply(data, "Depression", summarise, grp.mean=mean(HEALTH))
p9<-ggplot(data, aes(x=data[,30], color=Depression)) +
  geom_density()+
  labs(x = "General Health Status", y = "Density" ) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Depression),
             linetype="dashed")

mu<-ddply(data, "Depression", summarise, grp.mean=mean(REGDOC))
p10<-ggplot(data, aes(x=data[,31], color=Depression)) +
  geom_density()+
  labs(x = "Regular Doctor Visits", y = "Density" ) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Depression),
             linetype="dashed")

mu<-ddply(data, "Depression", summarise, grp.mean=mean(TREAT))
p11<-ggplot(data, aes(x=data[,32], color=Depression)) +
  geom_density()+
  labs(x = "Treatment for smoking, special diet, exercise, or drinking", y = "Density" ) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Depression),
             linetype="dashed")

mu<-ddply(data, "Depression", summarise, grp.mean=mean(BEDDAYS))
p12<-ggplot(data, aes(x=data[,33], color=Depression)) +
  geom_density()+
  labs(x = "Bed Days", y = "Density" ) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Depression),
             linetype="dashed")

mu<-ddply(data, "Depression", summarise, grp.mean=mean(ACUTEILL))
p13<-ggplot(data, aes(x=data[,34], color=Depression)) +
  geom_density()+
  labs(x = "Acute Illness", y = "Density" ) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Depression),
             linetype="dashed")

mu<-ddply(data, "Depression", summarise, grp.mean=mean(CHRONILL))
p14<-ggplot(data, aes(x=data[,35], color=Depression)) +
  geom_density()+
  labs(x = "Chronic Illness", y = "Density" ) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Depression),
             linetype="dashed")

mu<-ddply(data, "Depression", summarise, grp.mean=mean(C1))
p15<-ggplot(data, aes(x=data[,8], color=Depression)) +
  geom_density()+
  labs(x = "C1", y = "Density" ) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Depression),
             linetype="dashed")


grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 4)
grid.arrange(p9, p10, p11, p12, p13, p14, nrow = 3)

## Step 2 - t.test
out_sample=which(data$CASES==1)
t.test(data$SEX[out_sample],data$SEX[-out_sample])
t.test(data$AGE[out_sample],data$AGE[-out_sample])
t.test(data$MARITAL[out_sample],data$MARITAL[-out_sample])
t.test(data$EDUCAT[out_sample],data$EDUCAT[-out_sample])
t.test(data$EMPLOY[out_sample],data$EMPLOY[-out_sample])
t.test(data$INCOME[out_sample],data$INCOME[-out_sample])
t.test(data$RELIG[out_sample],data$RELIG[-out_sample])
t.test(data$DRINK[out_sample],data$DRINK[-out_sample])
t.test(data$HEALTH[out_sample],data$HEALTH[-out_sample])
t.test(data$REGDOC[out_sample],data$REGDOC[-out_sample])
t.test(data$TREAT[out_sample],data$TREAT[-out_sample])
t.test(data$BEDDAYS[out_sample],data$BEDDAYS[-out_sample])
t.test(data$ACUTEILL[out_sample],data$ACUTEILL[-out_sample])
t.test(data$CHRONILL[out_sample],data$CHRONILL[-out_sample])
t.test(data$C20[out_sample],data$C20[-out_sample])

## Step 3  - summary statistics
# # convert categorical variables to dummies
# data$MARITAL=as.factor(data$MARITAL)
# data$EDUCAT=as.factor(data$EDUCAT)
# data$EMPLOY=as.factor(data$EMPLOY)
# data$RELIG=as.factor(data$RELIG)
# data$HEALTH=as.factor(data$HEALTH)

# convert to binary of 0 and 1 (instead of 1 and 2)
data$SEX<-ifelse(data$SEX==1,0,1) #male=0, female=1
data$DRINK<-ifelse(data$DRINK==2,0,1) #not regular drinker=0, regular drinker=1
data$REGDOC<-ifelse(data$REGDOC==2,0,1) #no regular physician=0, regular physicain=1
data$TREAT<-ifelse(data$TREAT==2,0,1) #no treatment history=0, treatment history=1

out_sample=which(data$CASES==1)

data=data[,-28] #remove CASES
data=data[,-35] #remove data$Depression
data=data[,-c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)] #remove 20 depression scale items
data_backup=data

# scale data for step 4!
data_scaled=scale(data)
data_scaled=as.data.frame(data_scaled)

summary(data_scaled)

#column means of people with depression vs no depression
xbar1=colMeans(data_scaled[out_sample,])
xbar2=colMeans(data_scaled[-out_sample,])
xbar1
xbar2

#covariances of people with depression vs no depression
S1=cov(data_scaled[out_sample,])
S2=cov(data_scaled[-out_sample,])
S1
S2
diag(S1)
diag(S2)

#averaging the 2x2 pooled covariance matrix
Sp=(S1+S2)/2
Sp

rho1=cor(data_scaled[out_sample,])
rho2=cor(data_scaled[-out_sample,])

library(corrplot)
corrplot(rho1, method = "number")
corrplot(rho2, method = "number")

# density curve
plot(density(data_scaled[out_sample,]$INCOME),xlim=c(min(data[out_sample,]$INCOME),max(data[out_sample,]$INCOME)))
lines(density(data_scaled[-out_sample,]$INCOME),col=2)

## Step 4 - Calculate the linear combination and weights
# y=(xbar1-xbar2)%*%solve(Sp)%*%t(as.matrix(data_scaled))
# y=as.vector(y)
# y[out_sample]
# y[-out_sample]
# a=t((xbar1-xbar2)%*%solve(Sp))
# a
# astar=abs(a)/norm(a)
# astar
# ystar=t(astar)%*%t(as.matrix(data_scaled))
# ystar=as.vector(ystar)
# ystar[out_sample]
# ystar[-out_sample]

# plot(density(y[out_sample]), main="Linear Combination by Group", col=2)
# lines(density(y[-out_sample]),col=1)
# legend(10, 0.13, legend=c("Depression", "No Depression"), col=c("red", "black"), lty=1:1, cex=0.8)

# plot(density(ystar[out_sample]), main="Linear Combination by Group", col=2)
# lines(density(ystar[-out_sample]),col=1)
# legend(0.7, 1, legend=c("Depression", "No Depression"), col=c("red", "black"), lty=1:1, cex=0.8)

n_persons=dim(data_scaled)[1] # number of observations
x_terms=rep(0,n_persons) # a vector of 0 with length n
for (i in 1:n_persons){ # for each sample observation vector
  person_i=as.matrix(data_scaled[i,]) # x_0: specific patient's info
  x_terms[i] = person_i%*%solve(S1-S2)%*%t(person_i) +
    2*t(solve(S2)%*%xbar2-solve(S1)%*%xbar1)%*%t(person_i)
}

x_terms
x_terms[out_sample]
x_terms[-out_sample]

## Step 5 - Calculate cutoff and classify
#equal covariance, pop ratio, and cost ratio
# cutoff=.5*(xbar1-xbar2)%*%solve(Sp)%*%(xbar1+xbar2)
# cutoff=as.vector(cutoff)
# classify=ifelse(ystar>cutoff,0,1)
# cutoff
# 
# abline(v=cutoff)


##quadratic discriminant function
n_persons=dim(data_scaled)[1] # number of observations
det=rep(0,n_persons) # a vector of 0 with length n
k = xbar1%*%solve(S1)%*%xbar1-xbar2%*%solve(S2)%*%xbar2 + log(det(S2)/det(S1)) + 2*log(p2/p1)   # 1 x 1
for (i in 1:n_persons){ # for each sample observation vector
  person_i=as.matrix(data_scaled[i,]) # x_0: specific patient's info
  det[i] = person_i%*%solve(S1-S2)%*%t(person_i) +
            2*t(solve(S2)%*%xbar2-solve(S1)%*%xbar1)%*%t(person_i) + k
}

det

## our cutoff for different cost & population ratios
# #cost ratio: fp-$100, fn-$300
# p1=nrow(data[out_sample,])
# p2=nrow(data[-out_sample])
# fn=300
# fp=100
# cutoff=log((fp/fn)*(p2/p1))
# cutoff

cutoff = 0

classify = ifelse(detr>cutoff,1,0) # depressed if det > cutoff
classify

depress
sum(ifelse(classify == depress, 1, 0))/n_persons # percent of correct classifications

plot(density(det[out_sample]),xlim=c(-280,100),ylim=c(0,.02), main="Quadratic Discriminant Function", sub="Cutoff=0", col=2)
lines(density(det[-out_sample]),col=1)
legend(15, .02, legend=c("Depression", "No Depression"), col=c("red", "black"), lty=1:1, cex=0.65)
abline(v=cutoff)

## Step 6 - Inspect Results and calculate separation
dep=c(rep(1,294))
dep[out_sample]=2

plot(left, rep(1,length(left)), col=dep, xlab="", ylab="", main="Separation")
abline(v=cutoff)
legend(5, 1.35, legend=c("No Depression", "Depression"), col=c("black", "red"), lty=1:1, cex=0.6)

depress
classify

# Step 7 - Calculate upperbound and separation
# upper=(xbar1-xbar2)%*%solve(Sp)%*%((xbar1-xbar2))
# upper
# sy=(sum((ystar[out_sample]-mean(ystar[out_sample]))^2)+sum((ystar[-out_sample]-mean(ystar[-out_sample]))^2))/(length(out_sample)+length(-out_sample)-2)
# separation=abs(mean(ystar[out_sample])-mean(ystar[-out_sample]))/sy
# separation
# 
# t(a)%*%Sp%*%a


## Step 8 - Calculate accuracy metrics
library(forecast)
acc=c_accuracy(depress,classify)

library(ROCR)
pred<-prediction(det,depress)
perf <- performance(pred,"tpr","fpr")
plot(perf, main="ROC Curve")

### other , 
install.packages("dplyr")
library(MASS)
# model=lda(admired~-1+as.matrix(data))
# summary(model)
# predict(model)

#for quadratic discriminant analysis
qda.model=qda(depress~-1+as.matrix(data_scaled))
summary(qda.model)
names(qda.model)
qda.pred <- predict(qda.model)

qda.model$prior
qda.model$scaling
qda.model$ldet

qda.pred$class
qda.pred$posterior

acc <- length(which(qda.preds$class == depress))/length(depress)
print(paste0("Accuracy of QDA is: " , round(acc, 4)))
table(depress, qda.pred$class, dnn = c('Actual Group','Predicted Group'))

data_scaled %>%
  mutate(qda.pred = (qda.preds$class)) %>%
  summarise(qda.error = mean(depress != qda.pred))

acc2=c_accuracy(depress,qda.pred$class)
acc2

prediction(qda.pred$posterior[,2], depress) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot(main="ROC Curve")

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
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,tp,tn,fp,fn)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn")
  
  #print(scores)
  return(scores);
}



