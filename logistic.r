
## Step 0  - Read in Data
data=read.csv("casestudydata.csv")
names(data)
data=data[,-1]  ## remove ID

## Step 1 - Explore and relabel Data
y=data$CKD
class(data)
summary(data)
out_sample=which(is.na(data$CKD)==1)
data_out=data[out_sample,]   ## the ones without a disease status
data_in=data[-out_sample,]   ## the ones with a disease status
summary(data_in)

data_in=na.omit(data_in)

## Step 2  - Run the Logistic Regression with one variable

names(data)
model=glm(CKD~Age,family="binomial",data=data_in)
summary(model)
# the coeffienct of age is positive indicating that an increase in age is associated
#  with an increase in the probability of someone having CKD


## Step 3  - Run the Logistic Regression on all data, explore backward elimination
dim(data)
model=glm(CKD~.,family="binomial",data=data_in)
summary(model)
# notice the logistic regression automatically separated the categorical variables
#  if a category is not listed, it is included in the intercept

#model2=step(model,direction="forward")
#summary(model2)
model3=step(model,direction="backward")
# this will run the ENTIRE model with all variables, and then remove one at a time according to a 
#  p-value of the coefficient. it will result in only those variables "which matter"
#   ** note - some consider this unethical because you see the results before you select variables

## Step 4 - Explore your new model
formula(model3)
summary(model3)

# confidence intervals of the model coefficients (should not include 0 if p-value<.05)
confint.default(model3)
confint(model)

## Step 5 - Hypotehsis test of model, Compare 2 models, Definition 5-3
## difference in deviance  *this is sort of like R-squared but different because we use 0 or 1 only.
with(model3, null.deviance - deviance)
##df
with(model3, df.null - df.residual)
## pvalue of difference
with(model3, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
# if <.05, then model is significant from a null model (model with no variables)
# note that you can do this incrementally by adding one variable at a time.

## Step 5 - Alternate. Ho:  Model Fits the Data, Ha: Model does not Fit, Definition 5-2
## devniance
-2*logLik(model)
## test
with(model3, pchisq(deviance, df.residual, lower.tail = FALSE))



## Step 6 - Predict probabilities and Odds Ratios of New Data
## predictions of new data
newdata1=data_out[1:4,]  ## these are 4 "new patients"
newdata1  ## uh oh, 1 has missing data! damn!
phatnew=predict(model3, newdata = newdata1, type = "response")
# notice it only gives you 3 probabilities because 1 has missing data 
#    (you will have to fix that)

## odds ratios
phatnew/(1-phatnew)
# see if you can interpret these. it's the prob. of it happening/prob. of it not happening
#  related to gambling and betting on something

## Step 7 - Predict and Plot in-sample data
phat3=predict(model3,type="response")  # predicts for ALL in sample data
summary(phat3)  # probabilities range from .01% to 83.4%   - cool!


# let's compare that to a "simple" model with only age
model=glm(CKD~Age,family="binomial",data=data_in)
Age=seq(0,100,by=.1)
phat=predict(model,list(Age = Age),type="response")
summary(phat)  # range from 0.02% to 76.2%, it's similar but doesn't fit as well
plot(data_in$Age, data_in$CKD, pch = 16, xlab = "Age", ylab = "CKD")
lines(Age, phat)

## plot the actual probabilities for the "complicated" model
plot(data_in$Age, data_in$CKD, pch = 16, xlab = "Age", ylab = "CKD")
points(data_in$Age, phat3,col="blue")  # this plots all phat3's according to age

## Step 8 - Classification
summary(phat3)
classify=ifelse(phat3>.5,1,0)  # this is a threshold, we say if probability >50% , then say "yes"
summary(classify)  # notice that not many are "yes"  - is this desirable?

c_accuracy(data_in$CKD,classify)  # to run this you must run my code below first.

# notice these are the accuracy results when you use 50% as the threshold. they are kind of extreme
#    the false positive rate is almost 0% BECAUSE you almost NEVER say "yes"
#         true positive rate is 13.9% which isn't that bad because you have almost no false positives

## Step 9 - Caclculate Costs
acc=c_accuracy(data_in$CKD,classify)
c1=100   # penalize me  $100 for a false positive
c2=200  #  penalize me $200 for a false negatives
cost=acc[9]*c1+acc[10]*c2

cost   ## my costs are $48,800  because I got hit with a ton of false negative costs 
        # i said no too much!  many FN.  hmmm, 

## YOu must change the threshold of 50% above , to lower your costs.
##    How to do this?  One way is to search over 101 thresholds (each % from 0 to 100)

##  you may realize at some point, that plotting an ROC curve with roc()  gives you all possibilities
##   that's a high level understanding
install.packages('pROC')
library(pROC)
pROC(data_in$CKD,phat3)
model=roc(data_in$CKD~phat3,percent=TRUE,plot=TRUE)

## AUC is actually the probability of ranking a randomly chosen diseased person
##    higher than a randomly chosen non-disease person   (using the predictions from your model)

## wow, that's a good model!



## Function Below, RUN THIS FIRST
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



