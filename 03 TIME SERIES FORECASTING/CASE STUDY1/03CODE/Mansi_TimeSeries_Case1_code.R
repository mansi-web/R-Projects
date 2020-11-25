#time series forecasting
#case study
list.of.packages <- c("forecast", "ggplot2","MASS","caTools","sqldf","tseries")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(forecast)
library(tseries)
path<-"C:/Users/user/Desktop/IVY/R/05 PREDICTIVE ANALYTICS PROJECTS/05 TIME SERIES FORECASTING/CASE STUDY2/02DATA"
setwd(path)
data<-read.csv("1sales.csv",header = TRUE)
data1<-data
head(data1)
dim(data1)
str(data1)
summary(data1)
colSums(is.na(data1))
tsdata<-ts(data1[,2],frequency = 12,start = c(2003,1))
StructTS(tsdata)
str(tsdata)
tsdata
plot(tsdata,ylab="sales",xlab="year",main="Sales between 2003-2017",col="black")
abline(reg = lm(tsdata~time(tsdata))) #shows an increasing trend
cycle(TSdata)
plot(aggregate(tsdata,FUN=mean),xlab="year",ylab="mean sales",main="mean sales between 2003-17")
plot(log10(tsdata),ylab="log(Sales)",xlab="Year",main="log(Sales) between 2003-2017",col="grey")
##Differencing the data to remove trend
plot(diff(tsdata,differences = 1),ylab="Diff(Sales)",xlab="Year",main="Diff(Sales) between 2003-2017",col="grey")
plot(diff(tsdata,differences = 2),ylab="Diff(Sales)",xlab="Year",main="Diff(Sales) between 2003-2017",col="grey")
##
plot(diff(log10(tsdata),differences = 1),ylab="Diff(Sales)",xlab="Year",main="Diff(Log(Sales)) between 2003-2017",col="grey")
## ADF
LDTSdata=diff(log10(tsdata),differences = 2)
adf.test(LDTSdata,alternative="stationary")
#Since, the p-value <0.05, hence, we reject the Ho: Series is Non-Stationary 

kpss.test(LDTSdata)
####
## ACF anf PACF 
# ACF gives q (MA)
# PACF gives p (AR)
par(mfrow=c(1,2))
acf(diff(log10(tsdata),differences = 2),main="ACF plot")#ACF PLOT -- Moving Average or q
pacf(LDTSdata,main="PACF plot")#PACF PLOT -- Auto Regressive or p
#### ARIMA
ARIMAFit1=arima((log10(tsdata)),c(0,2,0))
summary(ARIMAFit1)
## AUTO ARIMA
ARIMAFit2=auto.arima(log10(tsdata),approximation=TRUE,trace=TRUE)
summary(ARIMAFit2)
#Predicting the future values
pred=predict(ARIMAFit2,n.ahead=36)
pred #result
#plotting the observed and forecasted data together
#Predicting the future values
par(mfrow=c(1,1))
options(scipen = 100)
plot(tsdata,type="l",xlim=c(2003,2020),xlab="Year",ylab="Sales",main="forecasted plot")
lines(10^(pred$pred),col="red")
#plotting the +-2 standard error to range of expected error
plot(tsdata,type="l",xlim=c(2003,2020),xlab = "Year",ylab = "Sales")
lines(10^(pred$pred),col="red")
lines(10^(pred$pred+2*pred$se),col="blue")
lines(10^(pred$pred-2*pred$se),col="green")
#normal result
normal_result=10^pred$pred ## you get the desired result. 
plot(normal_result,type="l",xlim=c(2017,2020),xlab = "Year",ylab = "Sales",main="final result")

