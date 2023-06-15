wine<- read.csv('E:/wine-cluster.csv',header= TRUE,sep=',')
wine

str(wine)
head(wine)
summary(wine)
#normalize dataset
normali<- function(x){(x-min(x))/(max(x)-min(x))}
wine_norm<- as.data.frame(lapply(wine[,c(1,2,3,4)],normali))
wine_norm

#checking if normalization is successful by checking min and max value of dataset
summary(wine_norm)

#Data splitting into training and test dataset
random<-sample(1:nrow(wine_norm),0.7*nrow(wine_norm))

wine_train<-wine_norm[random,] #70% training data
wine_test<-wine_norm[-random,] #remaining 30% test data

#target attribute k aladha kortesi
wine_train_labels<- wine[random,5]
wine_test_labels<-wine[-random,5]

#to use KNN, we need class package
install.packages("class")
library(class)

#model built
ml<-knn(wine_train,wine_test,cl=wine_train_labels,k=5)

View(wine)

#performance evaluation by confusion matrix
tab<- table(ml,wine_test_labels)
tab
#kmeans clustering

any(is.na(wine))
wine_scale <- scale(wine[-1]) 
dim(wine_scale)
head(wine_scale,3)
install.packages("cluster")
library(cluster)
install.packages("factoextra")
library(factoextra)
install.packages("ggplot2")
library(ggplot2)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(wine_scale, nc=10)


k.means <- kmeans(wine_scale, 3,nstart = 25) # k = 4
k.means
# Clusters
k.means$cluster
library(cluster)
clusplot(wine_scale, k.means$cluster, main='2D representation of the Cluster',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)