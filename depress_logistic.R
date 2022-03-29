
## Step 0  - Read in Data
data=read.csv("depres.csv")
names(data)
data=data[,-c(1,2)]  ## remove obs and ID

## Step 1 - Explore and relabel Data
summary(data)
y=data$CASES
class(data)

data = data[,-c(28)] # omit CESD

# convert categorical variables to dummies
data$MARITAL=as.factor(data$MARITAL)
data$EDUCAT=as.factor(data$EDUCAT)
data$EMPLOY=as.factor(data$EMPLOY)
data$RELIG=as.factor(data$RELIG)
data$HEALTH=as.factor(data$HEALTH)

# convert to binary of 0 and 1 (instead of 1 and 2)
data$SEX<-ifelse(data$SEX==1,0,1) #male=0, female=1
data$DRINK<-ifelse(data$DRINK==2,0,1) #not regular drinker=0, regular drinker=1
data$REGDOC<-ifelse(data$REGDOC==2,0,1) #no regular physician=0, regular physicain=1
data$TREAT<-ifelse(data$TREAT==2,0,1) #no treatment history=0, treatment history=1

summary(data)

#data_in=na.omit(data_in) # no missing data

#####PLOTS#####
# #by sex
# df=ifelse(data$SEX[data$CASES==1]==1,"M","F")
# df2=ifelse(data$SEX[data$CASES==0]==1,"M","F")
# data_perc <- t(prop.table(table(df))) * 100
# data_perc2 <- t(prop.table(table(df2))) * 100
# 
# a <- rbind(data_perc,data_perc2)
# colnames(a) <- c("F", "M")
# rownames(a) <- c("DEPRESSED", "NOT DEPRESSED")
# 
# barplot(height=a, beside = TRUE, legend.text = TRUE, main="Distribution of Sex", col=c("blue", "orange"), ylab = "Percent", , ylim=c(0,100), xlab="Sex")
# abline(h = c(20,40,60,80), col = "grey", lty = "dotted")
# barplot(height=a, beside = TRUE, legend.text = TRUE, main="Distribution of Sex", col=c("blue", "orange"), ylab = "Percent", , ylim=c(0,100), xlab="Sex", add=TRUE)
# 
# #by age
# h <- hist(data$AGE[(data$CASES==1)], breaks=c(10,20,30,40,50,60,70,80), plot=FALSE)
# h$density = h$counts/sum(h$counts) * 100
# plot(h, main="Distribution of Age Groups Among Depression Population",
#      xlab="Age Group",
#      ylab="Percent",
#      col="blue",
#      freq=FALSE)
# abline(h = c(10,20,30), col = "grey", lty = "dotted")
# plot(h, main="Distribution of Age Groups Among Depression Population",
#      xlab="Age Group",
#      ylab="Percent",
#      col="blue",
#      freq=FALSE,
#      add=TRUE)

## Step 2  - Run the Logistic Regression with one variable
model=glm(CASES~AGE,family="binomial",data=data)
summary(model)

model=glm(CASES~SEX,family="binomial",data=data)
summary(model)

model=glm(CASES~MARITAL,family="binomial",data=data)
summary(model)

model=glm(CASES~EDUCAT,family="binomial",data=data)
summary(model)

model=glm(CASES~EMPLOY,family="binomial",data=data)
summary(model)

model=glm(CASES~RELIG,family="binomial",data=data)
summary(model)

model=glm(CASES~DRINK,family="binomial",data=data)
summary(model)

model=glm(CASES~HEALTH,family="binomial",data=data)
summary(model)

model=glm(CASES~REGDOC,family="binomial",data=data)
summary(model)

model=glm(CASES~TREAT,family="binomial",data=data)
summary(model)

model=glm(CASES~BEDDAYS,family="binomial",data=data)
summary(model)

model=glm(CASES~ACUTEILL,family="binomial",data=data)
summary(model)

model=glm(CASES~CHRONILL,family="binomial",data=data)
summary(model)

model=glm(CASES~C20,family="binomial",data=data)
summary(model)


## Step 3  - Run the Logistic Regression on all data, explore backward elimination
summary(data)
dim(data)
model=glm(CASES~AGE+SEX+MARITAL+EDUCAT+EMPLOY+INCOME+RELIG+DRINK+HEALTH+REGDOC+TREAT
          +BEDDAYS+ACUTEILL+CHRONILL,family="binomial",data=data)
summary(model)
# notice the logistic regression automatically separated the categorical variables
#  if a category is not listed, it is included in the intercept

#model2=step(model,direction="forward")
#summary(model2)
model2=step(model,direction="backward")
# this will run the ENTIRE model with all variables, and then remove one at a time according to a 
#  p-value of the coefficient. it will result in only those variables "which matter"
#   ** note - some consider this unethical because you see the results before you select variables

## Step 4 - Explore your new model
formula(model2)
summary(model2)

# confidence intervals of the model coefficients (should not include 0 if p-value<.05)
confint.default(model2)
confint(model2)

## Step 5 - Hypotehsis test of model, Compare 2 models, Definition 5-3
## difference in deviance  *this is sort of like R-squared but different because we use 0 or 1 only.
with(model2, null.deviance - deviance)
##df
with(model2, df.null - df.residual)
## pvalue of difference
with(model2, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
# if <.05, then model is significant from a null model (model with no variables)
# note that you can do this incrementally by adding one variable at a time.

## Step 5 - Alternate. Ho:  Model Fits the Data, Ha: Model does not Fit, Definition 5-2
## devniance
-2*logLik(model2)
## test
with(model2, pchisq(deviance, df.residual, lower.tail = FALSE))


#############Combined model################
model=glm(CASES~SEX+EMPLOY+INCOME+RELIG+HEALTH+BEDDAYS,family="binomial",data=data)
summary(model)

confint.default(model)
confint(model)
with(model, null.deviance - deviance)
with(model, df.null - df.residual)
with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
-2*logLik(model)
with(model, pchisq(deviance, df.residual, lower.tail = FALSE))


# only significant vars
model=glm(CASES~EMPLOY+INCOME+RELIG+BEDDAYS,family="binomial",data=data)
summary(model)

confint.default(model)
confint(model)
with(model, null.deviance - deviance)
with(model, df.null - df.residual)
with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
-2*logLik(model)
with(model, pchisq(deviance, df.residual, lower.tail = FALSE))


# FINAL MODEL
model=glm(CASES~EMPLOY+INCOME+BEDDAYS,family="binomial",data=data)
summary(model)

confint.default(model)
confint(model)
with(model, null.deviance - deviance)
with(model, df.null - df.residual)
with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
-2*logLik(model)
with(model, pchisq(deviance, df.residual, lower.tail = FALSE))




## Step 6 - Predict probabilities and Odds Ratios of New Data
## predictions of data without depression status
newdata=data[,-28] 
summary(newdata)
phatnew=predict(model, newdata = newdata, type = "response")

## odds ratios
oddrat=phatnew/(1-phatnew)
# the prob. of it happening/prob. of it not happening
#  related to gambling and betting on something

##Randomly select individuals from depression and no depression group for predicted prob comparison
yes_dep=which(data$CASES==1)
no_dep=which(data$CASES==0)

data_dep=data[yes_dep,]
data_nodep=data[no_dep,]

data_dep=data_dep[sample(nrow(data_dep), 5), ]
data_nodep=data_nodep[sample(nrow(data_nodep), 5), ]
write.csv(rbind(data_dep, data_nodep), file = "sample.csv")

phatnew[c(104,144,251,201,69,102,242,49,286,249)]
oddrat[c(104,144,251,201,69,102,242,49,286,249)]


## Step 7 - Predict and Plot in-sample data
phat=predict(model,type="response")  # predicts for ALL in sample data
summary(phat)


# let's compare that to a "simple" model with only age
model=glm(CASES~EMPLOY+INCOME+BEDDAYS,family="binomial",data=data)
phat=predict(model,type="response")
summary(phat)  # range from 0.01% to 72.7%

predicted.data <- data.frame(probability.of.depression=model$fitted.values, depression=data$CASES)
predicted.data <- predicted.data[order(predicted.data$probability.of.depression, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)
library(ggplot2)
library(cowplot)
ggplot(data=predicted.data, aes(x=rank, y=probability.of.depression), ) +
  geom_point(aes(color=depression), alpha=1, shape=4, stroke=2) +
  geom_smooth(formula = y ~ x, method="glm", se=FALSE, fullrange=TRUE, method.args = list(family=quasibinomial)) +
  xlab("Index") +
  ylab("Predicted Probabilities of Getting Depression")
#ggsave("depression_probs.pdf")


## Step 8 - Classification
summary(phat)
classify=ifelse(phat>.6,1,0)  # this is a threshold, we say if probability >60% , then say "yes"
summary(classify)  # notice that not many are "yes"  - is this desirable?

acc=c_accuracy(data$CASES,classify) 
acc


classify=ifelse(phat>.1,1,0)  # this is a threshold, we say if probability >60% , then say "yes"
summary(classify)  # notice that not many are "yes"  - is this desirable?

acc=c_accuracy(data$CASES,classify) 
acc


# visualize .6 cutoff (lowest point of the previous plot)
install.packages("devtools")
library(devtools)
install.packages("fs")
library(fs)
devtools::install_github('cttobin/ggthemr')
library(ggplot2)
library(ggthemr)

install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)

cm_info <- ConfusionMatrixInfo(data = predicted.data, predict = "probability.of.depression", 
                                actual = "depression", cutoff = .6 )
ggthemr("flat")
cm_info$plot

##grid search for optimal cutoff
# reset to default ggplot theme 
ggthemr_reset()

# user-defined different cost for false negative and false positive
cost_fp <- 100
cost_fn <- 600
roc_info <- ROCInfo( data = cm_info$data, predict = "predict", 
                     actual = "actual", cost.fp = cost_fp, cost.fn = cost_fn )
grid.draw(roc_info$plot)

## Step 9 - Caclculate Costs
acc=c_accuracy(data$CASES,classify)
c1=100   # penalize me  $100 for a false positive
c2=200  #  penalize me $200 for a false negatives
cost=acc[9]*c1+acc[10]*c2

cost


##  you may realize at some point, that plotting an ROC curve with roc()  gives you all possibilities
##   that's a high level understanding
install.packages('pROC')
library(pROC)
roc(data$CASES,phat)
model=roc(data$CASES~phat,percent=TRUE,plot=TRUE)

rocobj <- roc(data$CASES, phat)
auc <- round(auc(data$CASES, phat),4)
ggroc(rocobj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))


## AUC is actually the probability of ranking a randomly chosen diseased person
##    higher than a randomly chosen non-disease person   (using the predictions from your model)



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


install.packages("prediction")
library(prediction)
library(ROCR)
library(grid)
library(caret)
library(dplyr)
library(scales)
library(ggplot2)
library(gridExtra)
library(data.table)

ConfusionMatrixInfo <- function( data, predict, actual, cutoff )
{	
  # extract the column ;
  # relevel making 1 appears on the more commonly seen position in 
  # a two by two confusion matrix	
  predict <- data[[predict]]
  actual  <- relevel( as.factor( data[[actual]] ), "1" )
  
  result <- data.table( actual = actual, predict = predict )
  
  # caculating each pred falls into which category for the confusion matrix
  result[ , type := ifelse( predict >= cutoff & actual == 1, "TP",
                            ifelse( predict >= cutoff & actual == 0, "FP", 
                                    ifelse( predict <  cutoff & actual == 1, "FN", "TN" ) ) ) %>% as.factor() ]
  
  # jittering : can spread the points along the x axis 
  plot <- ggplot( result, aes( actual, predict, color = type ) ) + 
    geom_violin( fill = "white", color = NA ) +
    geom_jitter( shape = 1 ) + 
    geom_hline( yintercept = cutoff, color = "blue", alpha = 0.6 ) + 
    scale_y_continuous( limits = c( 0, 1 ) ) + 
    scale_color_discrete( breaks = c( "TP", "FN", "FP", "TN" ) ) + # ordering of the legend 
    guides( col = guide_legend( nrow = 2 ) ) + # adjust the legend to have two rows  
    ggtitle( sprintf( "Confusion Matrix with Cutoff at %.2f", cutoff ) )
  
  return( list( data = result, plot = plot ) )
}

ROCInfo <- function( data, predict, actual, cost.fp, cost.fn )
{
  # calculate the values using the ROCR library
  # true positive, false postive 
  pred <- prediction( data[[predict]], data[[actual]] )
  perf <- performance( pred, "tpr", "fpr" )
  roc_dt <- data.frame( fpr = perf@x.values[[1]], tpr = perf@y.values[[1]] )
  
  # cost with the specified false positive and false negative cost 
  # false postive rate * number of negative instances * false positive cost + 
  # false negative rate * number of positive instances * false negative cost
  cost <- perf@x.values[[1]] * cost.fp * sum( data[[actual]] == 0 ) + 
    ( 1 - perf@y.values[[1]] ) * cost.fn * sum( data[[actual]] == 1 )
  
  cost_dt <- data.frame( cutoff = pred@cutoffs[[1]], cost = cost )
  
  # optimal cutoff value, and the corresponding true positive and false positive rate
  best_index  <- which.min(cost)
  best_cost   <- cost_dt[ best_index, "cost" ]
  best_tpr    <- roc_dt[ best_index, "tpr" ]
  best_fpr    <- roc_dt[ best_index, "fpr" ]
  best_cutoff <- pred@cutoffs[[1]][ best_index ]
  
  # area under the curve
  auc <- performance( pred, "auc" )@y.values[[1]]
  
  # normalize the cost to assign colors to 1
  normalize <- function(v) ( v - min(v) ) / diff( range(v) )
  
  # create color from a palette to assign to the 100 generated threshold between 0 ~ 1
  # then normalize each cost and assign colors to it, the higher the blacker
  # don't times it by 100, there will be 0 in the vector
  col_ramp <- colorRampPalette( c( "green", "orange", "red", "black" ) )(100)   
  col_by_cost <- col_ramp[ ceiling( normalize(cost) * 99 ) + 1 ]
  
  roc_plot <- ggplot( roc_dt, aes( fpr, tpr ) ) + 
    geom_line( color = rgb( 0, 0, 1, alpha = 0.3 ) ) +
    geom_point( color = col_by_cost, size = 4, alpha = 0.2 ) + 
    geom_segment( aes( x = 0, y = 0, xend = 1, yend = 1 ), alpha = 0.8, color = "royalblue" ) + 
    labs( title = "ROC", x = "False Postive Rate", y = "True Positive Rate" ) +
    geom_hline( yintercept = best_tpr, alpha = 0.8, linetype = "dashed", color = "steelblue4" ) +
    geom_vline( xintercept = best_fpr, alpha = 0.8, linetype = "dashed", color = "steelblue4" )				
  
  cost_plot <- ggplot( cost_dt, aes( cutoff, cost ) ) +
    geom_line( color = "blue", alpha = 0.5 ) +
    geom_point( color = col_by_cost, size = 4, alpha = 0.5 ) +
    ggtitle( "Cost" ) +
    scale_y_continuous( labels = comma ) +
    geom_vline( xintercept = best_cutoff, alpha = 0.8, linetype = "dashed", color = "steelblue4" )	
  
  # the main title for the two arranged plot
  sub_title <- sprintf( "Cutoff at %.2f - Total Cost = %d, AUC = %.3f", 
                        best_cutoff, best_cost, auc )
  
  # arranged into a side by side plot
  plot <- arrangeGrob( roc_plot, cost_plot, ncol = 2, 
                       top = textGrob( sub_title, gp = gpar( fontsize = 16, fontface = "bold" ) ) )
  
  return( list( plot 		  = plot, 
                cutoff 	  = best_cutoff, 
                totalcost   = best_cost, 
                auc         = auc,
                sensitivity = best_tpr, 
                specificity = 1 - best_fpr ) )
}

