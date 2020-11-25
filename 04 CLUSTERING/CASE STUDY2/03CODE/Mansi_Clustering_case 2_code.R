# CLUSTERING CASE 2
# STOCKS RETURN
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
path<-("C:/Users/user/Desktop/IVY/R/05 PREDICTIVE ANALYTICS PROJECTS/02 CLUSTERING/CASE STUDY2/02DATA")
setwd(path)
## ques 1
stocks<-read.csv("StocksCluster.csv", header = TRUE, as.is = TRUE, na.strings = c(""))
str(stocks)
summary(stocks)
## there are 11580 observations in the dataset.
## ques 2
library(dplyr)
data<-filter(stocks,PositiveDec==1)
a=nrow(data)/nrow(stocks)
a
# 0.546114 is the proportio of data having positive returns in Decemeber

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

##########
## QUES 4 logistic regression modelling
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
stocks$PositiveDec<-as.factor(stocks$PositiveDec)

num <- stocks[,(1:12)]#Numerical Data Frame
str(num)
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
a1<- IVCal(1,12,num,groups=10)
a2<- IVCal(2,12,num,groups=10)
a3<- IVCal(3,12,num,groups=10)
a4<- IVCal(4,12,num,groups=10)
a5<- IVCal(5,12,num,groups=10)
a6<- IVCal(6,12,num,groups=10)
a7<- IVCal(7,12,num,groups=10)
a8<- IVCal(8,12,num,groups=10)
a9<- IVCal(9,12,num,groups=10)
a10<- IVCal(10,12,num,groups=10)
a11<- IVCal(11,12,num,groups=10)
IV_num<- data.frame(rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11))
IV_num
######### train and test
set.seed(123)
spl = sample.split(stocks$PositiveDec, 0.75)
traindata = subset(stocks, spl == TRUE)
str(traindata)
dim(traindata)


testdata = subset(stocks, spl == FALSE)
str(testdata)
dim(testdata)

########### Modelling
Stocksmodel1<-glm(PositiveDec~ ReturnJan+ ReturnFeb+ ReturnMar+ ReturnApr+ ReturnMay+ ReturnJune+ ReturnJuly+ ReturnAug+ ReturnSep+ ReturnOct+ ReturnNov,data = traindata, family= binomial())
summary(Stocksmodel1)

Stocksmodel2<-glm(PositiveDec~ ReturnJan+ ReturnFeb+ ReturnApr+ ReturnMay+ ReturnJune+ ReturnJuly+ ReturnSep+ ReturnOct+ ReturnNov,data = traindata, family= binomial())
summary(Stocksmodel2)

##test on model
vif(Stocksmodel2)
#wald test
wald.test(b=coef(Stocksmodel2), Sigma= vcov(Stocksmodel2), Terms=1:9)
# Difference betweene null deviance and deviance
modelChi <- Stocksmodel2$null.deviance - Stocksmodel2$deviance
modelChi

# Coefficients (Odds)
Stocksmodel2$coefficients
# Coefficients (Odds Ratio)
exp(Stocksmodel2$coefficients)

varImp(Stocksmodel2)

# Predicted Probabilities
prediction <- predict(Stocksmodel2,newdata = traindata,type="response")
prediction

rocCurve   <- roc(response = traindata$PositiveDec, predictor = prediction, 
                  levels = rev(levels(traindata$PositiveDec)))
traindata$PositiveDec <- as.factor(traindata$PositiveDec)

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = traindata$PositiveDec)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)

###### KS statistics calculation
traindata$m1.yhat <- predict(Stocksmodel2,traindata, type = "response")
m1.scores <- prediction(traindata$m1.yhat, traindata$PositiveDec)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit
## 0.129

########## CLUSTERING

stocks1<-select(stocks,-PositiveDec)
#categorical variable removed from the data, for clustering
## no normalizing required here, since all data are in same units
distan<-dist(stocks1, method = "euclidean")
Clusterstocks<-hclust(distan, method = "ward.D")
plot(Clusterstocks) 

stockcluster<-cutree(Clusterstocks, k = 5)
table(stockcluster)

stockcluster<-cutree(Clusterstocks, k = 4)
table(stockcluster)

stockcluster<-cutree(Clusterstocks, k = 6)
table(stockcluster)
#cluster=6 seems fine here

#k-Means Clustersing
set.seed(88)

#Finding and Visualizing out the various clusters
stockcluster_K1<-kmeans(stocks1, centers = 4,iter.max = 1000)
stockcluster_K2<-kmeans(stocks1, centers = 5,iter.max = 1000)
stockcluster_K3<-kmeans(stocks1, centers = 6,iter.max = 1000)
stockcluster_K4<-kmeans(stocks1, centers = 7,iter.max = 1000)

#k=6 seems fine by this method as well

p1 <- fviz_cluster(stockcluster_K1, geom = "point", data = stocks1) + ggtitle("k = 4")
p2 <- fviz_cluster(stockcluster_K2, geom = "point",  data = stocks1) + ggtitle("k = 5")
p3 <- fviz_cluster(stockcluster_K3, geom = "point",  data =stocks1) + ggtitle("k = 6")
p4 <- fviz_cluster(stockcluster_K4, geom = "point",  data =stocks1) + ggtitle("k = 7")

grid.arrange(p1, p2, p3, p4, nrow = 2)

#ques 5
# Determing optimal numbers of clusters using the Elbow Method
set.seed(88)
fviz_nbclust(stocks1, kmeans, method = "wss")

k<-6
stockcluster_K<-kmeans(stocks1, centers = k,iter.max = 1000)
stockcluster_K
# 6 seems optimum.
table(stockcluster_K$cluster)
stockcluster_K$centers
fviz_cluster(stockcluster_K, data = stocks1)
# post clustering, 6th cluster seems to have the max no of data points.
