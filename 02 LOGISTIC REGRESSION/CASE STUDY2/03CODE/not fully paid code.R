#case 2: Logistic Regression: Loan repayment
#problem statemnet: to predict which kind of customers are able to pay the loan on time
packageUrl="https://cran.r-project.org/src/contrib/Archive/BaylorEdPsych/BaylorEdPsych_0.5.tar.gz"
install.packages(packageUrl,repos = NULL, type = "source")
library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
library(aod)
library(BaylorEdPsych)
library(ResourceSelection)
library(pROC)
library(ROCR)
library(caTools)
path<-"C:/Users/user/Desktop/IVY/R/05 PREDICTIVE ANALYTICS PROJECTS/03 LOGISTIC REGRESSION/CASE STUDY2/02DATA"
setwd(path)
data<-read.csv("loans_imputed.csv")
data1<-data
str(data1)
summary(data1)
dim(data1)
#factor variables
data1$credit.policy<-as.factor(data1$credit.policy)
data1$purpose<-as.factor(data1$purpose)
data1$not.fully.paid<-as.factor(data1$not.fully.paid)
str(data1)
#missing values
data.frame(colSums(is.na(data1)))
#Num and cat variables 
colnames(data1)
num <- data1[,c(3,4,5,6,7,8,9,10,11,12,13,14)]#Numerical Data Frame
cat <- data1[,c(1,2,14)]#Categorical Data Frame
str(num)
str(cat)
#####################
IVCal <- function(variable, target,data1,groups)
{
  data1[,"rank"] <- cut2(data1[,variable],g=groups)
  tableOutput <-sqldf(sprintf("select rank, 
                              count(%s) n,
                              sum(%s) good
                              from data1 
                              group by rank",target,target))
  tableOutput <- sqldf("select *,
                       (n - good) bad
                       from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)*100
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)*100
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))*100
  tableOutput$IV <- (log(tableOutput$good_rate/tableOutput$bad_rate))*(tableOutput$good_rate-tableOutput$bad_rate)/100
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

a1<- IVCal(3,14,num,groups=10)
a2<- IVCal(4,14,num,groups=10)
a3<- IVCal(5,14,num,groups=10)
a4<- IVCal(6,14,num,groups=10)
a5<- IVCal(7,14,num,groups=10)
a6<- IVCal(8,14,num,groups=10)
a7<- IVCal(9,14,num,groups=10)
a8<- IVCal(10,14,num,groups=10)
a9<- IVCal(11,14,num,groups=10)
a10<-IVCal("delinq_2yrs","nfp",num,groups=10)
a11<-IVCal("pub_rec","nfp",num,groups=10)

IV_num<- data.frame(rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11))
IV_num
str(data1)
colnames(data1)[colnames(data1)=="not.fully.paid"]<-"nfp"
colnames(data1)[colnames(data1)=="pub.rec"]<-"pub_rec"
colnames(data1)[colnames(data1)=="delinq.2yrs"]<-"delinq_2yrs"

#################
CA <- function(target, variable, data1) 
{
  A1<- fn$sqldf("select $variable,count($target)n, sum($target)good from data group by $variable")
  
  A1<- fn$sqldf("select *, (n-good) bad from A1")
  A1$bad_rate <- A1$bad/sum(A1$bad)*100
  
  A1$good_rate<- A1$good/sum(A1$good)*100
  A1$WOE<- (log(A1$good_rate/A1$bad_rate))*100
  A1$IV <- (log(A1$good_rate/A1$bad_rate))*(A1$good_rate-A1$bad_rate)/100
  IV <- sum(A1$IV[is.finite(A1$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

A<- CA(14,1,cat)
B<- CA(14,"purpose",cat)

IV_cat<- data.frame(rbind(A,B))
IV_cat
Final_IV <- data.frame(rbind(IV_num,IV_cat))
Final_IV

##################### train test
set.seed(123)
spl = sample.split(data1$nfp, 0.75)
traindata = subset(data1, spl == TRUE)
str(traindata)
dim(traindata)


testdata = subset(data1, spl == FALSE)
str(testdata)
dim(testdata)

########## modelling
model1<-glm(nfp~.,data = traindata, family= binomial())
summary(model1)

model2<-glm(nfp~ credit.policy+ I(purpose=="credit_card")+ I(purpose=="debt_consolidation")+ I(purpose=="major_purchase")+ I(purpose=="small_business")+ installment+ log.annual.inc+ fico+ revol.bal+ dti+ days.with.cr.line+ revol.util+ inq.last.6mths+ delinq_2yrs+ pub_rec,data = traindata, family= binomial())
summary(model2)

model3<-glm(nfp~ credit.policy+ I(purpose=="credit_card")+ I(purpose=="debt_consolidation")+ I(purpose=="major_purchase")+ I(purpose=="small_business")+ installment+ log.annual.inc+ fico+ revol.bal+ revol.util+ inq.last.6mths+ pub_rec,data = traindata,family = binomial())
summary(model3)

############ tests on final model
vif(model3)
#wald test
wald.test(b=coef(model3), Sigma= vcov(model3), Terms=1:5)
# Difference betweene null deviance and deviance
modelChi <- model3$null.deviance - model3$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- model3$df.null - model3$df.residual
chidf

# Coefficients (Odds)
model3$coefficients
# Coefficients (Odds Ratio)
exp(model3$coefficients)


# Variable Importance of the model
varImp(model3)

# Predicted Probabilities
prediction <- predict(model3,newdata = traindata,type="response")
prediction

##################
PseudoR2(model3)

###########
rocCurve   <- roc(response = traindata$nfp, predictor = prediction, 
                  levels = rev(levels(traindata$nfp)))
traindata$nfp <- as.factor(traindata$nfp)

#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best",transpose=TRUE)[1],1,0)
Confusion <- table(Predicted = predclass,Actual = traindata$nfp)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best",transpose=TRUE),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve, legacy.axes=TRUE)



########### KS statistics calculation
traindata$m1.yhat <- predict(model3,traindata, type = "response")
m1.scores <- prediction(traindata$m1.yhat, traindata$nfp)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 0.4 - 0.7


########################
ROCRpred<-prediction(prediction,traindata$nfp)
ROCRpref<-performance(ROCRpred,"tpr","fpr")
plot(ROCRpref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

############
predclass <-ifelse(prediction>coords(rocCurve,"best",transpose=TRUE)[1],1,0)
Confusion <- table(Predicted = predclass,Actual = traindata$nfp)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1
AUCmetric <- data.frame(c(coords(rocCurve,"best",transpose=TRUE),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

##############
#Testing on the test data
modeltest1<-glm(nfp~.,data = testdata,family = binomial())
summary(modeltest1)

modeltest2<-glm(nfp~ credit.policy+ purpose+ installment+ log.annual.inc+ dti+ fico+ revol.bal+ inq.last.6mths+ delinq_2yrs+ pub_rec,data = testdata,family = binomial() )
summary(modeltest2)

modeltest3<-glm(nfp~ credit.policy+ purpose+ installment+ log.annual.inc+ dti+ fico+ revol.bal+ inq.last.6mths,data = testdata,family = binomial() )
summary(modeltest3)

modeltest4<-glm(nfp~ credit.policy+ I(purpose=="home_improvement")+ I(purpose=="small_business")+ installment+ log.annual.inc+ dti+ fico+ revol.bal+ inq.last.6mths+ delinq_2yrs+ pub_rec,data=testdata,family = binomial())
summary(modeltest4)

modeltest5<-glm(nfp~ credit.policy+ I(purpose=="home_improvement")+ I(purpose=="small_business")+ installment+ log.annual.inc+ dti+ fico+ revol.bal+ inq.last.6mths,data=testdata,family = binomial())
summary(modeltest5)

vif(modeltest5)

# Difference between -2LL of Null model and model with variables
modelChi <- modeltest5$null.deviance - modeltest5$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- modeltest5$df.null - modeltest5$df.residual
chidf

# Coefficients (Odds)
modeltest5$coefficients
# Coefficients (Odds Ratio)
exp(modeltest5$coefficients)

# Predicted Probabilities
prediction <- predict(modeltest5,newdata = testdata,type="response")
prediction

rocCurve   <- roc(response = testdata$nfp, predictor = prediction, 
                  levels = rev(levels(testdata$nfp)))
testdata$nfp <- as.factor(testdata$nfp)


#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best",transpose=TRUE)[1],1,0)
Confusion <- table(Predicted = predclass,Actual = testdata$nfp)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best",transpose=TRUE),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)



############
### KS statistics calculation
testdata$m1.yhat <- predict(modeltest5, testdata, type = "response")

library(ROCR)
m1.scores <- prediction(testdata$m1.yhat, testdata$nfp)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40 - 70

##########################
