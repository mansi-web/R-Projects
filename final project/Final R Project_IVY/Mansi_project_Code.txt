## R FINAL PROJECT
## TO PREDICT CLV FOR AN AUTO INSURANCE COMPANY
## SUBMITTED BY: MANSI
# MULTIVARIATE LINEAR REGRESSION MODELLING
library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)
library(tidyverse)
path<-("C:/Users/user/Desktop/IVY/03 R/final project/Final R Project_IVY")
setwd(path)
data<-read.csv("finalprojectdata.csv",stringsAsFactors = TRUE)
View(data)
data1<-data
# exploring data
str(data1)
summary(data1)
dim(data1)
# changing data types
data1$Customer<-as.character(data1$Customer)
data1$Effective.To.Date<-as.character(data1$Effective.To.Date)
# renaming columns
colnames(data1)<-str_replace_all(colnames(data1),"[.]","_")
colnames(data1)[which(names(data)=="Customer.Lifetime.Value")]="clv"
# outlier treatment
quantile(data1$clv,sort(c(seq(0,1,0.05),0.99,0.995)))
#creating a new data frame with a cap on outliers
data2=data1[data1$clv <35971,]
nrow(data1)-nrow(data2)
# 92 rows removed
quantile(data2$clv,sort(c(seq(0,1,0.05),0.99,0.995)))
data3=data2[data2$clv <20468, ]
# 452 more rows removed
summary(data3)
# data is more normally distributed now, with less difference between mean and median
# checking for missing values
colSums(is.na(data3))
# no missing values
# Dropping the rendundant variables
as.data.frame(colnames(data3))
data4=select(data3,-c(Customer,Effective_To_Date))
#final dataset for modelling
str(data4)
# 22 variables, 8590 rows
# train and validation(test) data
set.seed(123)
spl = sample.split(data4$clv, 0.7)
#Splits the overall data into train and test data in 70:30 ratio
traindata = subset(data4, spl == TRUE)
str(traindata)
dim(traindata)
# 6013 rows, 22 variables
testdata = subset(data4, spl == FALSE)
str(testdata)
dim(testdata)
# 2577 rows, 22 variables
# Modelling
model1<-lm(clv~.,data=traindata)
summary(model1)
model2<-lm(clv~Response+	Coverage+	EmploymentStatus+	Gender+	Income+	Marital_Status+	Monthly_Premium_Auto+	Months_Since_Policy_Inception+	Number_of_Open_Complaints+	Number_of_Policies + Policy+	Renew_Offer_Type+	Total_Claim_Amount+	Vehicle_Class+	Vehicle_Size, data=traindata)
summary(model2)
model3<-lm(clv~Coverage+	EmploymentStatus+	Income+	Marital_Status+	Monthly_Premium_Auto+	Months_Since_Policy_Inception+	Number_of_Open_Complaints+	Number_of_Policies + Policy+	Renew_Offer_Type+	Vehicle_Class,data=traindata)
summary(model3)
model4<-lm(clv~Coverage+	I(EmploymentStatus=="Employed")+ I(EmploymentStatus=="Unemployed")+	Income+	I(Marital_Status=="Single")+	Monthly_Premium_Auto+	Months_Since_Policy_Inception+	Number_of_Open_Complaints+	Number_of_Policies + I(Policy=="Corporate L3")+	Renew_Offer_Type+	I(Vehicle_Class=="Luxury Car")+ I(Vehicle_Class=="Sports Car")+ (Vehicle_Class=="SUV")+ (Vehicle_Class=="Two-Door Car"),data=traindata)
summary(model4)
model5<-lm(clv~ I(Coverage=="Premium")+	I(EmploymentStatus=="Employed")+	Income+	I(Marital_Status=="Single")+	Monthly_Premium_Auto+	Months_Since_Policy_Inception+	Number_of_Open_Complaints+	Number_of_Policies+	Renew_Offer_Type+ (Vehicle_Class=="SUV")+ (Vehicle_Class=="Two-Door Car"),data=traindata)
summary(model5)
# model 5 has all significant variables. This is the final model.
# checking for multi collinearity
vif(model5)
# vif less than 2.5 therefore no multi collinearity
# predicted or fitted values
fitted(model5)
# plot
par(mfrow=c(2,2))
plot(model5)
# new column created in train data with predicted clv values
traindata$pred<- fitted(model5)
write.csv(traindata,"train_predicted.csv")
# MAPE calculation
attach(traindata)
MAPE<-print((sum((abs(clv-pred))/clv))/nrow(traindata))
# MAPE value is 0.4277 which is pretty low signifying the good fit of the model
# checking for auto correlation
durbinWatsonTest(model5)
# or
dwt(model5)
# p value is 0.316
# p-value is >0.05, we fail to reject H0: (No Autocorrelation)
# checking for Heteroscedasticity
# Breusch-Pagan test
# Null hypothesis -> error is non-homogenious (p value should be more than 0.05)
bptest(model5)  
# p-value < 2.2e-16 (bad sign!)
# Normality testing 
# Null hypothesis is data is normal.
resids <- model5$residuals
ad.test(resids) #get Anderson-Darling test for normality 
cvm.test(resids) #get Cramer-von Mises test for normaility 
# p value is 7.37e-10
qqnorm(resids)
# testing the model on validation data(test data)
finalmodel1<-lm(clv~ I(Coverage=="Premium")+	I(EmploymentStatus=="Employed")+	Income+	I(Marital_Status=="Single")+	Monthly_Premium_Auto+	Months_Since_Policy_Inception+	Number_of_Open_Complaints+	Number_of_Policies+	Renew_Offer_Type+ (Vehicle_Class=="SUV")+ (Vehicle_Class=="Two-Door Car"),data=testdata)
summary(finalmodel1)
finalmodel2<-lm(clv~ I(Marital_Status=="Single")+	Monthly_Premium_Auto+	Number_of_Open_Complaints+	Number_of_Policies+	Renew_Offer_Type,data=testdata)
summary(finalmodel2)
finalmodel3<-lm(clv~ I(Marital_Status=="Single")+	Monthly_Premium_Auto+	Number_of_Open_Complaints+	Number_of_Policies+ I(Renew_Offer_Type=="Offer2")+ I(Renew_Offer_Type=="Offer4") ,data=testdata)
summary(finalmodel3)
# vif of final model
vif(finalmodel3)
# all vif values less than 1.02 i.e. less than 2, thus no multi collinearity
# finding out predicted values of clv in test data
fitted(finalmodel3)
# creating new column in test data
testdata$pred <- fitted(finalmodel3)
# creating csv file in WD with the predicted values
write.csv(testdata,"test_predicted.csv")
#Calculating MAPE
attach(testdata)
(sum((abs(clv-pred))/clv))/nrow(testdata)
# MAPE is 0.4411, thus a good fit model
# checking auto correlation using DW Test
durbinWatsonTest(finalmodel3)
# or
dwt(finalmodel3)
# p value is 0.496
# checking for Heteroscedasticity
# Breusch-Pagan test
bptest(finalmodel3)  # Null hypothesis -> error is homogenious (p value should be more than 0.05)
# p value is 2.2e-16 (bad sign)
# Normality testing: Null hypothesis is that our data is normally distributed.
resids1 <- finalmodel3$residuals
ad.test(resids1) #get Anderson-Darling test for normality 
cvm.test(resids1) #get Cramer-von Mises test for normaility 
# p value is 7.37e-10
par(mfrow=c(2,2))
qqnorm(resids)
qqnorm(resids1)
