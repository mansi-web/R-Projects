#case1
#K-Means Clustering Project
rm(list = ls())
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)



################################################################################
###### Build an analytical model to create clusters of airline travellers ######
################################################################################

#---------------------Step1: Loading the Data in R
Path<-setwd("C:/Users/user/Desktop/IVY/R/05 PREDICTIVE ANALYTICS PROJECTS/02 CLUSTERING/CASE STUDY1/02DATA")
airlines<-read.csv("AirlinesCluster.csv", header = TRUE, as.is = TRUE, na.strings = c(""))


#Understand the data type and summary of each coloumn
str(airlines)
summary(airlines)
#ques1
# Balance and BonusTrans have the largest values whereas all variables have almost the same minimumvalue.

#Checking missing values
as.data.frame(colSums(is.na(airlines)))

### removing outliers
quantile(airlines$Balance,sort(c(seq(0,1,0.05),0.99,0.995)))
airlines2=airlines[airlines$Balance <230787.2,]

quantile(airlines$QualMiles,sort(c(seq(0,1,0.05),0.99,0.995)))
airlines2=airlines[airlines$QualMiles <6057.96,]

quantile(airlines$BonusMiles,sort(c(seq(0,1,0.05),0.99,0.995)))
airlines2=airlines[airlines$BonusMiles<104182.3,]

quantile(airlines$FlightMiles,sort(c(seq(0,1,0.05),0.99,0.995)))
airlines2=airlines[airlines$FlightMiles<6568.64,]

#Normalizing the Data for clustering 
library(caret)
preproc<-preProcess(airlines2)
airlinesNorm<-predict(preproc,airlines2)
summary(airlinesNorm)
## ques 2
# after normalizing the data,Balance has the largest values, whereas DaysSinceEnroll has the minimum value.

#Hiearchical Clustering
## ques 3
distan<-dist(airlinesNorm, method = "euclidean")
ClusterAirline<-hclust(distan, method = "ward.D")
plot(ClusterAirline) #dendogram
# clusters between 4 to 7 seems fine here by looking at the dendogram

#ques4
#Assigning points to the clusters
AirlineCluster<-cutree(ClusterAirline, k = 5)
table(AirlineCluster)
## no of data points in cluster 1 = 1552

AirlineCluster1<-cutree(ClusterAirline, k = 4)
table(AirlineCluster1)

AirlineCluster2<-cutree(ClusterAirline, k = 7)
table(AirlineCluster2)


#Computing the average values of the cluster groups
## ques 5
MeanComp<-function(var, clustergrp, measure){
  z<-tapply(var, clustergrp, measure)
  print(z)
}
colnames(airlines2)
Bal_mean<-MeanComp(airlines2$Balance, AirlineCluster, mean)
Bal_mean<-MeanComp(airlines2$QualMiles, AirlineCluster, mean)
Bal_mean<-MeanComp(airlines2$BonusMiles, AirlineCluster, mean)
Bal_mean<-MeanComp(airlines2$BonusTrans, AirlineCluster, mean)
Bal_mean<-MeanComp(airlines2$FlightMiles, AirlineCluster, mean)
Bal_mean<-MeanComp(airlines2$FlightTrans, AirlineCluster, mean)
Bal_DaysSinceEnroll<-MeanComp(airlines2$DaysSinceEnroll, AirlineCluster, mean)

#Appending the Clusters Assignment
Airlines_H<-data.frame(airlines2,AirlineCluster)
write.csv(Airlines_H,"Airlines_Hierarchical.csv", row.names = FALSE)

######################################################################

#k-Means Clustersing
##ques6
set.seed(88)

#Finding and Visualizing out the various clusters
AirlineCluster_K1<-kmeans(airlinesNorm, centers = 4,iter.max = 1000)
AirlineCluster_K2<-kmeans(airlinesNorm, centers = 5,iter.max = 1000)
AirlineCluster_K3<-kmeans(airlinesNorm, centers = 6,iter.max = 1000)
AirlineCluster_K4<-kmeans(airlinesNorm, centers = 7,iter.max = 1000)

## when k=5, 2 clusters have data points>1000

# plots to compare
p1 <- fviz_cluster(AirlineCluster_K1, geom = "point", data = airlinesNorm) + ggtitle("k = 4")
p2 <- fviz_cluster(AirlineCluster_K2, geom = "point",  data = airlinesNorm) + ggtitle("k = 5")
p3 <- fviz_cluster(AirlineCluster_K3, geom = "point",  data = airlinesNorm) + ggtitle("k = 6")
p4 <- fviz_cluster(AirlineCluster_K4, geom = "point",  data = airlinesNorm) + ggtitle("k = 7")

grid.arrange(p1, p2, p3, p4, nrow = 2)

# Determing optimal numbers of clusters using the Elbow Method
set.seed(88)
fviz_nbclust(airlinesNorm, kmeans, method = "wss")

k<-5
AirlineCluster_K<-kmeans(airlinesNorm, centers = k,iter.max = 1000)
AirlineCluster_K

table(AirlineCluster_K$cluster)
AirlineCluster_K$centers
fviz_cluster(AirlineCluster_K, data = airlinesNorm)

#Finding out the Mean Values of the Variables in the Clusters
## ques 7
Bal_mean_k<-aggregate(airlines2, by=list(cluster=AirlineCluster_K$cluster), mean)
Bal_mean<-MeanComp(airlines2$Balance, AirlineCluster, mean)

#Appending the Clusters Assignment
airlines_new_k <- data.frame(airlines2, AirlineCluster_K$cluster)
write.csv(airlines_new_k,"Airlines_k-Means.csv", row.names = FALSE)

