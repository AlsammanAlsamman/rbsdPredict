# Clustering functions for RBSD project
library(ggplot2)
library(dplyr)
library(reshape2)
library(rstatix)
library(caret)
library(e1071)
library(randomForest)
library(Metrics)

# Data scaling and clustering
cluster_data <- function(data) {
  data_clustering <- data[, c("tmax", "tmin", "RH")]
  data_clustering_scaled <- scale(data_clustering)
  kmeans_result <- kmeans(data_clustering_scaled, centers = 3, nstart = 25)
  data_clustering$cluster <- kmeans_result$cluster
  return(data_clustering)
}

# Hierarchical clustering and heatmap
heatmap_cluster_test <- function(data, file) {
  ds <- dist(data)
  pdf(file)
  heatmap(as.matrix(ds))
  dev.off()
}

# Cluster assignment by rules
classify_point <- function(tmax, tmin, RH) {
  # Example: Euclidean distance to cluster means (replace with real means)
  cluster1_dist <- sqrt((tmax - 31.44)^2 + (tmin - 22.30)^2 + (RH - 88.10)^2)
  cluster2_dist <- sqrt((tmax - 35.52)^2 + (tmin - 24.37)^2 + (RH - 78.93)^2)
  cluster3_dist <- sqrt((tmax - 36.15)^2 + (tmin - 24.68)^2 + (RH - 66.90)^2)
  distances <- c(cluster1_dist, cluster2_dist, cluster3_dist)
  cluster <- which.min(distances)
  return(cluster)
}
