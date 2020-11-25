# CLUSTERING CASE 2
#STOCK RETURNS
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)
library(caret)
library(caTools)
library(dplyr)
library(formattable)
library(flexclust)
library(kableExtra)
library(knitr)
library(ggplot2)
library(ggcorrplot)
## ques 1
path<-("C:/Users/user/Desktop/IVY/R/05 PREDICTIVE ANALYTICS PROJECTS/02 CLUSTERING/CASE STUDY2/02DATA")
setwd(path)
stocks<-read.csv("StocksCluster.csv")
str(stocks)
summary(stocks)
## there are 11580 observations in the dataset.
## ques 2
data<-filter(stocks,PositiveDec==1)
a=nrow(data)/nrow(stocks)
a
## 0.5461 proportion of all the observations had positive returns in December
## ques 3 to find correlation
data.frame(cor(stocks)) %>% ## here stocks should have all numeric variables
  round(3) %>%
  formattable(list(
    area(col=1:12,row=1:12)~
      color_tile("white","red")
  ))
## or using 2nd method to find correlations
data<-select(stocks, - PositiveDec)
correlation<-as.matrix(round(cor(data),2))
colors()
ggcorrplot(correlation,type="upper",lab=TRUE,lab_size = 3,colors = c("yellow","white","magenta3"),ggtheme = theme_bw(),method = "square",title = "Correlation between monthly returns")
## test and train data
set.seed(123)
spl = sample.split(stocks$PositiveDec, 0.75)
traindata = subset(stocks, spl == TRUE)
str(traindata)
dim(traindata)

testdata = subset(stocks, spl == FALSE)
str(testdata)
dim(testdata)

## removing dec variable

stockstrain<-select(traindata,-PositiveDec) ## train without Dec
stockstest<-select(testdata,-PositiveDec)   ## test without Dec
# normalizing the data
preproc<-preProcess(stockstrain)
normtrain<-predict(preproc,stockstrain)
normtest<-predict(preproc,stockstest)
summary(normtrain)
summary(normtest)

## k means clustering



















