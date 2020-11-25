#MLRM CASE 2
#To predict monthly sales of Hyundai Elantra in the United States
path<-"C:/Users/user/Desktop/IVY/R/05 PREDICTIVE ANALYTICS PROJECTS/01 MULTIVARIATE LINEAR REGRESSION/CASE STUDY2/02DATA"
setwd(path)
data<-read.csv("elantra.csv")
data1<-data
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
str(data1)
summary(data1)
dim(data1)
quantile(data1$ElantraSales,sort(c(seq(0,1,0.05),0.99,0.995)))
#ques1
traindata<-subset(data1,data1$Year<=2012)
dim(traindata)
testdata<-subset(data1,data1$Year>2012)
dim(testdata)
#ques2
model1<-lm(ElantraSales~ Unemployment+ CPI_all+ CPI_energy+ Queries,data = traindata)
summary(model1)
#R squared value of model is 0.3544
#ques3
#Using 0.10 as p-value cutoff
#none of the variables are significant as p>0.10 for all
#ques4
coef(summary(model1))["Unemployment","Estimate"]
#or
summary(model1)$coefficients["Unemployment","Estimate"]
#coefficient of unemployement is (-3179.9) which means that for every one unit increase in unemployment, the predicted sales go down by 3179 units.
#ques5
model2<-lm(ElantraSales~ Month+ Unemployment+ Queries+ CPI_all+ CPI_energy,data = traindata)
summary(model2)
#New R squared value is 0.3402
#ques6
traindata$Month<-as.factor(traindata$Month)
model3<-lm(ElantraSales~ Month+ Unemployment+ Queries+ CPI_all+ CPI_energy,data = traindata)
summary(model3)
#Now  R squared value is 0.6837
#ques7
#Month,CPI_all,CPI_energy,Unemployment variables are significant as p<0.1
#ques8
testdata$Month<-as.factor(testdata$Month)
predictions<-predict(model3,newdata = testdata)
View(predictions)
sumof_sq_error<-sum((predictions-testdata$ElantraSales)^2)
sumof_sq_error
#ques9
#test set R squared is 1-(sumof_sq_error / sum of sq diff btwn elantra sales in test and mean of sales in train )
sumof_sq_diff_btw_sales_in_test_and_mean_in_train<-sum((testdata$ElantraSales-mean(traindata$ElantraSales))^2)
1-((sumof_sq_error)/sumof_sq_diff_btw_sales_in_test_and_mean_in_train)
#R squared value is now 0.7426902
#ques10
max(abs(predictions-testdata$ElantraSales))
#Largest absolute error is 7632.811
testdata$Month[which.max(abs(predictions-testdata$ElantraSales))]
#largest absolute error is observed in the 3rd row
testdata
#3rd row shows month 2 and year 2013 i.e. feb 2013 is the pair for which max error in predicted value exists.

