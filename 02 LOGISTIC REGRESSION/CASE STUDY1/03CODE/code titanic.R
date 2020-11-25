#case 1 logistic regression- Titanic
#problem statement: To predict which sort of passengers survived the tragedy
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
path<-"C:/Users/user/Desktop/IVY/R/05 PREDICTIVE ANALYTICS PROJECTS/03 LOGISTIC REGRESSION/CASE STUDY1/02DATA"
setwd(path)
data<-read.csv("train.csv")
data1<-data
str(data1)
summary(data1)
dim(data1)
data1$Survived<-as.factor(data1$Survived)
data1$Pclass<-as.factor(data1$Pclass)
data1$SibSp<-as.factor(data1$SibSp)
data1$Parch<-as.factor(data1$Parch)
data1$Sex<-as.factor(data1$Sex)
data1$Embarked<-as.factor(data1$Embarked)
str(data1)
#missing values treatment
data.frame(colSums(is.na(data1)))
data1$Age[which(is.na(data1$Age))]<-mean(data1$Age,na.rm = TRUE)
data.frame(colSums(is.na(data1)))
is.na(data1$Age)
colnames(data1)
num <- data1[,c(2,6,10)]#Numerical Data Frame
cat <- data1[,c(2,3,5,7,8,12)]#Categorical Data Frame
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

a1<- IVCal("Age","Survived",num,groups=10)
a2<- IVCal("Fare","Survived",num,groups=10)

IV_num<- data.frame(rbind(a1,a2))
IV_num

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

A<- CA("Survived","Pclass",cat)
B<- CA("Survived","Sex",cat)
C<- CA("Survived","SibSp",cat)
D<- CA("Survived","Parch",cat)
E<- CA("Survived","Embarked",cat)

IV_cat<- data.frame(rbind(A,B,C,D,E))
IV_cat
Final_IV <- data.frame(rbind(IV_num,IV_cat))
Final_IV

######### train and test
set.seed(123)
spl = sample.split(data1$Survived, 0.75)
traindata = subset(data1, spl == TRUE)
str(traindata)
dim(traindata)


testdata = subset(data1, spl == FALSE)
str(testdata)
dim(testdata)

########### Modelling
data1$Name<-as.character(data1$Name)
model1<-glm(Survived~ Pclass+ Sex+ Age+SibSp+ Parch+ Fare+ Embarked,data = traindata, family= binomial())
summary(model1)

model2<-glm(Survived~ Pclass+ Sex+ Age+SibSp+ Parch,data = traindata, family= binomial())
summary(model2)

model3<-glm(Survived~ Pclass+ Sex+ Age+ I(SibSp==3)+ I(SibSp==4),data = traindata,family = binomial())
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

#########
PseudoR2(model3)

###########
rocCurve   <- roc(response = traindata$PositiveDec, predictor = prediction, 
                  levels = rev(levels(traindata$PositiveDec)))
traindata$PositiveDec <- as.factor(traindata$PositiveDec)

#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = traindata$nfp)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)



########### KS statistics calculation
traindata$m1.yhat <- predict(model3,traindata, type = "response")
m1.scores <- prediction(traindata$m1.yhat, traindata$nfp)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 0.4 - 0.7

####################
testmodel1<-glm(Survived~ Pclass+ Sex+ Age+SibSp+ Parch+ Fare+ Embarked,data = testdata, family= binomial())
summary(testmodel1)

testmodel2<-glm(Survived~ Pclass+ Sex+ Age+ Fare, data = testdata,family = binomial())
summary(testmodel2)

testmodel3<-glm(Survived~ Pclass+ Sex+ Age+ Fare, data = testdata,family = binomial())
summary(testmodel3)

vif(testmodel3)

modelChi <- testmodel3$null.deviance - testmodel3$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- testmodel3$df.null - testmodel3$df.residual
chidf

# Coefficients (Odds)
testmodel3$coefficients
# Coefficients (Odds Ratio)
exp(testmodel3$coefficients)

# Predicted Probabilities
prediction <- predict(testmodel3,newdata = testdata,type="response")
prediction

rocCurve   <- roc(response = testdata$Survived, predictor = prediction, 
                  levels = rev(levels(testdata$nfp)))
testdata$Survived<- as.factor(testdata$Survived)


#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best",transpose=TRUE)[1],1,0)
Confusion <- table(Predicted = predclass,Actual = testdata$Survived)
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
testdata$m1.yhat <- predict(testmodel3, testdata, type = "response")

library(ROCR)
m1.scores <- prediction(testdata$m1.yhat, testdata$Survived)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40 - 70
