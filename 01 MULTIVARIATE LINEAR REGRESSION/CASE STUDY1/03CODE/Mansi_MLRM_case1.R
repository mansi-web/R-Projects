#MLRM CASE STUDY 1
#to predict life expectancy by state using state statistics
#dependent variable is life expectancy
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
setwd("C:/Users/user/Desktop/IVY/R/05 PREDICTIVE ANALYTICS PROJECTS/01 MULTIVARIATE LINEAR REGRESSION/CASE STUDY1/02DATA")
data<-read.csv("statedata.csv")
data1<-data
str(data1)
dim(data1)
summary(data1)
colnames(data1)[colnames(data1)=="Life.Exp"]<-"lexp"
quantile(data1$lexp,sort(c(seq(0,1,0.05),0.99,0.995)))
boxplot(data1$lexp)
#no outliers detected
colSums(is.na(data1))
#no missing values
as.data.frame(colnames(data1))
data2<-select(data1,-c(state.abb,state.area,state.name,state.division))
dim(data2)
str(data2)
# iteration 1 on all variables
linearmodel1<-lm(lexp~.,data = data2)
summary(linearmodel1)
# iteration 2 
linearmodel2<-lm(lexp~ Population+ Income+ Illiteracy+ Murder+ Area+ Frost+ HS.Grad+ y+ I(state.region=="West")+ I(state.region=="Northeast"),data = data2)
summary(linearmodel2)
# iteration 3
linearmodel3<-lm(lexp~ Murder+ Area+ Frost+ HS.Grad+ I(state.region=="West")+ I(state.region=="Northeast"), data = data2)
summary(linearmodel3)
#iteration 4
linearmodel4<-lm(lexp~ Murder+ HS.Grad+ I(state.region=="West"),data = data2)
summary(linearmodel4)
#final model
linearmodelfinal<-lm(lexp~ Murder+ HS.Grad,data = data2)
summary(linearmodelfinal)
#Checking Multicollinearity in the model
vif(linearmodelfinal)
## Get the predicted or fitted values
fitted(linearmodelfinal)
plot(linearmodelfinal)
#MAPE
data2$predicted<-fitted(linearmodelfinal)
write.csv(data2,"mape.csv")
#mape csv file created in WD with a new column named predicted
attach(data2)
MAPE<-print((sum((abs(lexp-predicted))/lexp))/nrow(train_data))
# Checking for Assumptions
# auto correlation
dwt(linearmodelfinal)
# multi collinearity
vif(linearmodelfinal)
# Breusch-Pagan test
bptest(linearmodelfinal)
# ________________________________________#