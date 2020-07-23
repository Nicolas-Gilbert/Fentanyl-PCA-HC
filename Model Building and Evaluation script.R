# Load appropriate packages
library(factoextra)
library(FactoMineR)
library(fields)


### MODEL 1 CREATION ###
# Import and clean up data
data.class1 <- read.csv("Classifier_1.csv")
row.names(data.class1) <- data.class1[,1]
data.class1[1] <- NULL
colnames(data.class1) <- c(41:352)

# Remove data with variance below 0.0001
data.class1.var <- data.class1[,apply(data.class1, 2, var, na.rm=TRUE) > 0.0001]

# Execute PCA and HCPC
res.pca.class1 <- PCA(data.class1.var, ncp = 5)
res.hcpc.class1 <- HCPC(res.pca.class1, nb.clust = 9, consol = T)



### MODEL 2 CREATION ###
# Import and clean up datca
data.class2 <- read.csv("Classifier_2.csv")
row.names(data.class2) <- data.class2[,1]
data.class2[1] <- NULL
colnames(data.class2) <- c(41:352)

# Remove data with variance below 0.0001
data.class2.var <- data.class2[,apply(data.class2, 2, var, na.rm=TRUE) > 0.0001]

# Execute PCA and HCPC
res.pca.class2 <- PCA(data.class2.var, ncp = 4)
res.hcpc.class2 <- HCPC(res.pca.class2, nb.clust = 4)



### EVALUATION ###
# Import and clean up test data
test <- read.csv("Test_data.csv")
row.names(test) <- test[,1]
test[1] <- NULL
colnames(test) <- c(41:352)

### CLASSIFICATION 1 ###
# Subset same columns in test.class1 as in the model
columns <- c()
test.class1 <- test
for(value in colnames(test.class1)){if (value %in% colnames(data.class1.var)){ columns <- c(columns, value)} else {columns <- c(columns, NA)}}
x <- which(columns %in% NA)
test.class1[x] <- NULL

# Nearest neighbour classifier #1
data.test.class1 <- c()
test.result.class1 <- c()
j <- 1

while(j < nrow(test.class1) + 1)
{    
  data.test.class1 <- rbind(data.class1.var, test.class1[j,])
  
  #Generate PCA and HCPC dendrogram, making sure to set test value as a supplementary individual
  res.pca.test.class1 <- PCA(data.test.class1, ncp = ncol(res.hcpc.class1$call$X)-1, ind.sup = nrow(data.class1)+1)
  
  #Find centroid closest to sample
  x <- which.min(rdist(res.hcpc.class1$call$X[1:5], res.pca.test.class1$ind.sup$coord))
  result <- res.hcpc.class1$call$X[x,]
  test.result.class1 <- rbind(test.result.class1, result)
  j <- j + 1 
}

# Clean up result table
test.result.class1[,1] <- row.names(test.result.class1)
row.names(test.result.class1) <- row.names(test.class1)
test.result.class1 <- test.result.class1[, -c(2:5)]
colnames(test.result.class1) <- c("Nearest Neighbour", "Cluster")
test.result.class1


### CLASSIFICATION 2 ###
# Isolate individuals put into cluster 2 by Classifier 1

y <- which(test.result.class1[,2] == 2)
test.class2 <- test[y,]

# Subset same columns in test.class2 as in the model
columns <- c()

for(value in colnames(test.class2)){if (value %in% colnames(data.class2.var)){ columns <- c(columns, value)} else {columns <- c(columns, NA)}}
x <- which(columns %in% NA)
test.class2[x] <- NULL


# Nearest neighbour classifier #2
data.test.class2 <- c()
test.result.class2 <- c()
j <- 1

while(j < nrow(test.class2) + 1)
{    
  data.test.class2 <- rbind(data.class2.var, test.class2[j,])
  
  #Generate PCA and HCPC dendrogram, making sure to set test value as a supplementary individual
  res.pca.test.class2 <- PCA(data.test.class2, ncp = ncol(res.hcpc.class2$call$X)-1, ind.sup = nrow(data.class2)+1)
  
  #Find centroid closest to sample
  x <- which.min(rdist(res.hcpc.class2$call$X[1:4], res.pca.test.class2$ind.sup$coord))
  result <- res.hcpc.class2$call$X[x,]
  test.result.class2 <- rbind(test.result.class2, result)
  j <- j + 1 
}

# Clean up result table
test.result.class2[,1] <- row.names(test.result.class2)
row.names(test.result.class2) <- row.names(test.class2)
test.result.class2 <- test.result.class2[, -c(2:4)]
colnames(test.result.class2) <- c("Nearest Neighbour", "Cluster")
test.result.class2



### CENTROID CLASSIFICATION (IF NEEDED) ###

#Calculate cluster centroids for both classifiers
#Classifier 1
i <- 1
centroids.class1 <- c()

while(i < nlevels(res.hcpc.class1$call$X$clust) + 1)
{
  cluster.i <- res.hcpc.class1$call$X[which(res.hcpc.class1$call$X$clust %in% i),]
  
  j <- 1
  centroid.i <- c()
  
  while(j < ncol(cluster.i))
  {
    centroid.i <- cbind(centroid.i, mean(cluster.i[,j]))
    j <- j + 1
  }
  
  centroids.class1 <- rbind(centroids.class1, centroid.i)
  i <- i+1
}

row.names(centroids.class1) <- c(1:(i-1))



#Classifier 2
i <- 1
centroids.class2 <- c()

while(i < nlevels(res.hcpc.class2$call$X$clust) + 1)
{
  cluster.i <- res.hcpc.class2$call$X[which(res.hcpc.class2$call$X$clust %in% i),]
  
  j <- 1
  centroid.i <- c()
  
  while(j < ncol(cluster.i))
  {
    centroid.i <- cbind(centroid.i, mean(cluster.i[,j]))
    j <- j + 1
  }
  
  centroids.class2 <- rbind(centroids.class2, centroid.i)
  i <- i+1
}

row.names(centroids.class2) <- c(1:(i-1))


#Compare test data against centroids
# Classifier 1
data.test.class1 <- c()
test.result.class1 <- c()
j <- 1

while(j < nrow(test.class1) + 1)
{    
  data.test.class1 <- rbind(data.class1.var, test.class1[j,])
  
  #Generate PCA and HCPC dendrogram, making sure to set test value as a supplementary individual
  res.pca.test.class1 <- PCA(data.test.class1, ncp = ncol(res.hcpc.class1$call$X)-1, ind.sup = nrow(data.class1)+1)
  
  #Find centroid closest to sample
  result <- c(which.min(rdist(centroids.class1,res.pca.test.class1$ind.sup$coord)), min(rdist(centroids.class1, res.pca.test.class1$ind.sup$coord)))
  test.result.class1 <- rbind(test.result.class1, result)
  j <- j + 1 
}

row.names(test.result.class1) <- row.names(test.class1)
colnames(test.result.class1) <- c("Cluster", "Distance")
test.result.class1

# Classifier 2
data.test.class2 <- c()
test.result.class2 <- c()
j <- 1

while(j < nrow(test.class2) + 1)
{    
  data.test.class2 <- rbind(data.class2.var, test.class2[j,])
  
  #Generate PCA and HCPC dendrogram, making sure to set test value as a supplementary individual
  res.pca.test.class2 <- PCA(data.test.class2, ncp = ncol(res.hcpc.class2$call$X)-1, ind.sup = nrow(data.class2)+1)
  
  #Find centroid closest to sample
  result <- c(which.min(rdist(centroids.class2,res.pca.test.class2$ind.sup$coord)), min(rdist(centroids.class2, res.pca.test.class2$ind.sup$coord)))
  test.result.class2 <- rbind(test.result.class2, result)
  j <- j + 1 
}

row.names(test.result.class2) <- row.names(test.class2)
colnames(test.result.class2) <- c("Cluster", "Distance")
test.result.class2