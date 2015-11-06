#############################################################################

## Session: k-Means Clustering in R 
## Created By: Ujjwal Karn           
## Date: 05-Oct-2015                 

#############################################################################

# Topics Covered
# 
# 1. Reading data and Summary Statistics
# 2. Determining the Optimal Number of Clusters
# 3. Running Clustering Algorithm and Visualisations


##############################################################################
#Reading data and Summary Statistics

#change the working directory
setwd("C:\\Users\\ujjwal.karn\\Desktop\\Classification & Clustering")

mydata<-read.csv("data/kmeans_data.csv")

head(mydata)
str(mydata)
summary(mydata)

plot(mydata[c("Sepal.Length", "Sepal.Width")], main="Raw Data")

#standardising the data
mydata <- scale(mydata)

##############################################################################
#Determining the Optimal Number of Clusters
#http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters/

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

for(i in 1:25){wss[i] <- sum(kmeans(mydata, centers=i)$withinss)}

plot(1:25, wss, type="b", xlab="No. of Clusters", ylab="wss")

wss


##############################################################################
#Running Clustering Algorithm

# trying with 4 clusters
clus4 <- kmeans(mydata, centers=4, nstart=30)

#check between_SS / total_SS
clus4

# get cluster means 
aggregate(mydata ,by=list(clus4$cluster), FUN=mean)

# append cluster assignment
mydata <- data.frame(mydata, clus4$cluster)

#summary
groups <- data.frame(clus4$cluster)
table(groups)

plot(mydata[c("Sepal.Length", "Sepal.Width")], col=clus4$cluster)
points(clus4$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=2)


# trying with 3 clusters
clus3 <- kmeans(mydata, centers=3, nstart=20)
clus3

# get cluster means 
aggregate(mydata ,by=list(clus3$cluster), FUN=mean)

# append cluster assignment
mydata <- data.frame(mydata, clus3$cluster)

#summary
groups <- data.frame(clus3$cluster)
table(groups)

plot(mydata[c("Sepal.Length", "Sepal.Width")], col=clus3$cluster)
points(clus3$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=2)
