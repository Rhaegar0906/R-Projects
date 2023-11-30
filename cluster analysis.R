library(cluster)
library(readxl)
library(ggplot2)
library(factoextra)
library(dplyr)

# Load and View Data
myData <- read_excel("/Users/rhaeagrxing/Downloads/Data_cluster.xlsx")
View(myData)

# Data Preprocessing for Clustering
myData$Sex <- as.factor(myData$Sex)
myData$Married <- as.factor(myData$Married)
myData$College <- as.factor(myData$College)
myData$City <- as.factor(myData$City)

# Calculate Gower Distance
d <- daisy(myData[,1:7], metric='gower')  # Gower distances between records.
mResult <- agnes(d, method='ward')  # Generate clusters using Ward's method
plot(mResult)  # Dendrogram

# Determine Optimal Number of Clusters
fviz_nbclust(d, method="ward.D2", k.max=10, diss=TRUE, color='red')

# Perform Agglomerative Clustering
mClusters <- cutree(mResult, k=4)  # Select 4 clusters
myData <- data.frame(myData, mClusters)  # Add cluster indicator
summary(as.factor(mClusters))  # Cluster record counts
View(myData)

# Cluster Description and Analysis
for(i in 1:4) {
  cat("Cluster", i, "Summary:\n")
  print(summary(subset(myData, mClusters==i)))
  cat("\n")
}

# Visualization of Clusters
fviz_cluster(list(data=d, cluster=mClusters), geom="point", stand=FALSE)

# Example 14.3 - K-Medoids Clustering
myData2 <- read_excel("/Users/rhaeagrxing/Downloads/Data_cluster2.xlsx")
View(myData2)

# Standardize Data
myData2 <- scale(myData2[,1:3])

# Set Seed for Consistency
suppressWarnings(RNGversion("3.5.3"))
set.seed(1)

# K-Medoids Clustering
kResult <- pam(myData2, k=3)
summary(kResult)

# Generate Clustering Plots
fviz_cluster(kResult, geom="point", stand=FALSE)

# Silhouette Plot for Assessing Clustering Quality
fviz_silhouette(kResult)

# Variable Importance in Clustering
myData2 <- as.data.frame(myData2)
myData2$cluster <- as.factor(kResult$clustering)
variable_importance <- varclus(myData2[, -ncol(myData2)], clus=3)
plot(variable_importance)

# Save Updated Data
write.xlsx(myData, "/Users/rhaeagrxing/Downloads/Updated_Data_cluster.xlsx")
write.xlsx(myData2, "/Users/rhaeagrxing/Downloads/Updated_Data_cluster2.xlsx")

