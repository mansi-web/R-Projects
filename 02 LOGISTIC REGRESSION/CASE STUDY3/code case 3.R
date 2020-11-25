#logistic regression-Case 3
#default on payment 
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
library(dplyr)
path<-"C:/Users/user/Desktop/IVY/R/05 PREDICTIVE ANALYTICS PROJECTS/03 LOGISTIC REGRESSION/01CASE"
setwd(path)
data<-read.csv("Data_for_Logistic_Regression.csv", header = TRUE, stringsAsFactors = TRUE)
data1<-data
str(data1)
summary(data1)
dim(data1)
data.frame(colSums(is.na(data1)))
colnames(data1)[colnames(data1)=="Default_On_Payment"]<-"dop"
data1$Inst_Rt_Income<-as.factor(data1$Inst_Rt_Income)
data1$Current_Address_Yrs<-as.factor(data1$Current_Address_Yrs)
data1$Num_CC<-as.factor(data1$Num_CC)
data1$Dependents<-as.factor(data1$Dependents)
data2<-select(data1,-c(Customer_ID,Count))
str(data2)
## Data set with numeric and categorical variables
num <-data2[,c("dop","Age","Credit_Amount","Duration_in_Months")]#Numerical Data Frame
cat <-data2[,-c(2,5,13)] #Categorical Data Frame
head(cat)
head(num)
str(num)
str(cat)
########### IV Calculation for numerical data
IVCal <- function(variable, target,data,groups)
{
  data[,"rank"] <- cut2(data[,variable],g=groups)
  tableOutput <-sqldf(sprintf("select rank, 
                              count(%s) n,
                              sum(%s) good
                              from data 
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
a1<- IVCal("Age","dop",num,groups=10)
a2<- IVCal("Credit_Amount","dop",num,groups=10)
a3<- IVCal("Duration_in_Months","dop",num,groups=10)

IV_num<- data.frame(rbind(a1,a2,a3))
IV_num

############# Iv calculation for categorical variables
CA <- function(target, variable, data) {
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
A<- CA("dop","Status_Checking_Acc",cat)
B<- CA("dop","Credit_History",cat)
C<- CA("dop","Purposre_Credit_Taken",cat)
D<- CA("dop","Savings_Acc",cat)
E<- CA("dop","Years_At_Present_Employment",cat)
F<- CA("dop","Inst_Rt_Income",cat)

G<- CA("dop","Marital_Status_Gender",cat)
H<- CA("dop","Other_Debtors_Guarantors",cat)
I<- CA("dop","Current_Address_Yrs",cat)
J<- CA("dop","Property",cat)
K<- CA("dop","Other_Inst_Plans",cat)
L<- CA("dop","Housing",cat)
M<- CA("dop","Num_CC",cat)
N<- CA("dop","Job",cat)
O<- CA("dop","Dependents",cat)
P<- CA("dop","Foreign_Worker",cat)
IV_cat<- data.frame(rbind(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P))
IV_cat
Final_IV <- data.frame(rbind(IV_num,IV_cat))
Final_IV
############# train and test
set.seed(123)

spl = sample.split(data2$dop, 0.7)
traindata = subset(data2, spl == TRUE)
str(traindata)
dim(traindata)


testdata = subset(data2, spl == FALSE)
str(testdata)
dim(testdata)

########## Logistic Regression Model Building
model1<-glm(dop~.,data = traindata, family = "binomial")
summary(model1)

colnames(data2)

model2<-glm(dop~ Status_Checking_Acc+ Duration_in_Months+ Credit_History+ Purposre_Credit_Taken+ Credit_Amount+ Savings_Acc+ Inst_Rt_Income+ Marital_Status_Gender+ Other_Debtors_Guarantors+ Current_Address_Yrs+ Property+ Age+ Other_Inst_Plans+ Num_CC+ Telephone+ Foreign_Worker,data = traindata,family = "binomial")
summary(model2)

model3<-glm(dop~ Status_Checking_Acc+ Duration_in_Months+ Credit_History+ Purposre_Credit_Taken+ Credit_Amount+ Savings_Acc+ Inst_Rt_Income+ I(Marital_Status_Gender=="A93")+ Other_Debtors_Guarantors+ Current_Address_Yrs+ Property+ Age+ I(Other_Inst_Plans=="A143")+ I(Num_CC==2)+ Telephone+ Foreign_Worker,data = traindata,family = "binomial")
summary(model3)

model4<-glm(dop~ Status_Checking_Acc+ Duration_in_Months+ I(Credit_History=="A32")+ I(Credit_History=="A33")+ I(Credit_History=="A34")+ Purposre_Credit_Taken+ Credit_Amount+ I(Savings_Acc=="A62")+ I(Savings_Acc=="A64")+ I(Savings_Acc=="A65")+ I(Inst_Rt_Income==3)+ I(Inst_Rt_Income==4)+ I(Marital_Status_Gender=="A93")+ Other_Debtors_Guarantors+ Current_Address_Yrs+ Property+ Age+ I(Other_Inst_Plans=="A143")+ I(Num_CC==2)+ Telephone+ Foreign_Worker,data = traindata,family = "binomial")
summary(model4)

model5<-glm(dop~ Status_Checking_Acc+ Duration_in_Months+ I(Credit_History=="A32")+ I(Credit_History=="A33")+ I(Credit_History=="A34")+ I(Purposre_Credit_Taken=="A41")+ I(Purposre_Credit_Taken=="A410")+ I(Purposre_Credit_Taken=="A42")+ I(Purposre_Credit_Taken=="A43")+ I(Purposre_Credit_Taken=="A48")+ I(Purposre_Credit_Taken=="A49")+Credit_Amount+ I(Savings_Acc=="A62")+ I(Savings_Acc=="A64")+ I(Savings_Acc=="A65")+ I(Inst_Rt_Income==3)+ I(Inst_Rt_Income==4)+ I(Marital_Status_Gender=="A93")+ Other_Debtors_Guarantors+ Current_Address_Yrs+ Property+ Age+ I(Other_Inst_Plans=="A143")+ I(Num_CC==2)+ Telephone+ Foreign_Worker,data = traindata,family = "binomial")
summary(model5)

vif(model5)

#####using Wald Test
wald.test(b=coef(model5), Sigma= vcov(model5), Terms=1:21)

##########
# Difference betweene null deviance and deviance
modelChi <- model5$null.deviance - model5$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- model5$df.null - model5$df.residual
chidf


# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)

#### power of the model
PseudoR2(model5)

# Hosmer and Lemeshow given by the McFadden 6R square
R2.hl<-modelChi/model5$null.deviance
R2.hl

########
# Coefficients (Odds)
model5$coefficients
# Coefficients (Odds Ratio)
exp(model5$coefficients)


# Variable Importance of the model
varImp(model5)

# Predicted Probabilities
prediction <- predict(model5,newdata = traindata,type="response")
prediction

###### roc
rocCurve   <- roc(response = traindata$dop, predictor = prediction, 
                  levels = rev(levels(traindata$dop)))
traindata$dop <- as.factor(traindata$dop)

###########################
predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = traindata$dop)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

############
### KS statistics calculation
traindata$m1.yhat <- predict(model5, traindata, type = "response")
m1.scores <- prediction(traindata$m1.yhat, traindata$dop)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 0.4 - 0.7

Confusion 
plot(rocCurve)

###### modelling on test data

modeltest<-glm(dop~.,data = testdata,family = "binomial")
summary(modeltest)

modeltest<-glm(dop~ Status_Checking_Acc+ Duration_in_Months+ I(Credit_History=="A32")+ I(Credit_History=="A33")+ I(Credit_History=="A34")+ I(Purposre_Credit_Taken=="A41")+ I(Purposre_Credit_Taken=="A410")+ I(Purposre_Credit_Taken=="A42")+ I(Purposre_Credit_Taken=="A43")+ I(Purposre_Credit_Taken=="A44")+ I(Purposre_Credit_Taken=="A46")+ I(Purposre_Credit_Taken=="A48")+ I(Purposre_Credit_Taken=="A49")+ Credit_Amount+ I(Savings_Acc=="A63")+ I(Savings_Acc=="A64")+ I(Savings_Acc=="A65")+ I(Years_At_Present_Employment=="A74")+ I(Inst_Rt_Income==3)+ I(Inst_Rt_Income==4)+ I(Marital_Status_Gender=="A93")+ I(Other_Debtors_Guarantors=="A103")+ I(Current_Address_Yrs==2)+ I(Current_Address_Yrs==3)+ I(Property=="A124")+ Age+ I(Other_Inst_Plans=="A143")+ I(Housing=="A152")+ I(Job=="A172") +I(Job=="A173")+ I(Foreign_Worker=="A202"),data = testdata,family = "binomial")
summary(modeltest)

modeltest<-glm(dop~ Status_Checking_Acc+ Duration_in_Months+ I(Credit_History=="A32")+ I(Credit_History=="A33")+ I(Credit_History=="A34")+ I(Purposre_Credit_Taken=="A41")+ I(Purposre_Credit_Taken=="A410")+ I(Purposre_Credit_Taken=="A42")+ I(Purposre_Credit_Taken=="A43")+ I(Purposre_Credit_Taken=="A44")+ I(Purposre_Credit_Taken=="A46")+ I(Purposre_Credit_Taken=="A48")+ I(Purposre_Credit_Taken=="A49")+ Credit_Amount+ I(Savings_Acc=="A63")+ I(Savings_Acc=="A64")+ I(Savings_Acc=="A65")+ I(Years_At_Present_Employment=="A74")+ I(Inst_Rt_Income==3)+ I(Inst_Rt_Income==4)+ I(Marital_Status_Gender=="A93")+ I(Other_Debtors_Guarantors=="A103")+ I(Current_Address_Yrs==2)+ I(Current_Address_Yrs==3)+ Age+ I(Housing=="A152")+ I(Job=="A172") +I(Job=="A173")+ I(Foreign_Worker=="A202"),data = testdata,family = "binomial")
summary(modeltest)

modeltest<-glm(dop~ Status_Checking_Acc+ Duration_in_Months+ I(Credit_History=="A32")+ I(Credit_History=="A33")+ I(Credit_History=="A34")+ I(Purposre_Credit_Taken=="A41")+ I(Purposre_Credit_Taken=="A410")+ I(Purposre_Credit_Taken=="A42")+ I(Purposre_Credit_Taken=="A43")+ I(Purposre_Credit_Taken=="A44")+ I(Purposre_Credit_Taken=="A46")+ I(Purposre_Credit_Taken=="A48")+ I(Purposre_Credit_Taken=="A49")+ Credit_Amount+ I(Savings_Acc=="A63")+ I(Savings_Acc=="A64")+ I(Savings_Acc=="A65")+ I(Years_At_Present_Employment=="A74")+ I(Inst_Rt_Income==3)+ I(Inst_Rt_Income==4)+ I(Marital_Status_Gender=="A93")+ I(Other_Debtors_Guarantors=="A103")+ I(Current_Address_Yrs==2)+ I(Current_Address_Yrs==3)+ I(Housing=="A152")+ I(Foreign_Worker=="A202"),data = testdata,family = "binomial")
summary(modeltest)

vif(modeltest)

########################################
# Difference between -2LL of Null model and model with variables
modelChi <- modeltest$null.deviance - modeltest$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- modeltest$df.null - modeltest$df.residual
chidf

chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)


# Hosmer and Lemeshow R square
R2.hl<-modelChi/modeltest$null.deviance
R2.hl


# Cox and Snell R Square

R.cs <- 1 - exp ((modeltest$deviance - modeltest$null.deviance) /5000)
R.cs

# Max rescaled R square (Nagelkarke) (the last number; here is 2000 should be total no. of ovservation)

R.n <- R.cs /(1-(exp(-(modeltest$null.deviance/5000))))
R.n

library(ResourceSelection)
hl <- hoslem.test(testdata$dop, fitted(modeltest), g=10)
hl
#######
Coefficients (Odds)
modeltest$coefficients
# Coefficients (Odds Ratio)
exp(modeltest$coefficients)

# Predicted Probabilities
prediction <- predict(modeltest,newdata = testdata,type="response")
prediction

rocCurve   <- roc(response = testdata$dop, predictor = prediction, 
                  levels = rev(levels(testdata$dop)))
testdata$dop <- as.factor(testdata$dop)


#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = testdata$dop)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)

### KS statistics calculation
testdata$m1.yhat <- predict(modeltest, testdata, type = "response")

library(ROCR)
m1.scores <- prediction(testdata$m1.yhat, testdata$dop)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit 