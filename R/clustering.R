# Clustering functions for RBSD project
library(ggplot2)
library(dplyr)
library(reshape2)
library(rstatix)
library(caret)
library(e1071)
library(randomForest)
library(Metrics)

#' Perform k-means clustering
#'
#' @param data A data frame with at least 'tmax', 'tmin', and 'RH' columns.
#' @param num_centers The number of clusters to form.
#' @param predictors A character vector of predictor column names.
#' @return A list containing the original data with a 'cluster' column,
#'         and the kmeans model object.
cluster_data <- function(data, num_centers = 3, predictors = c("tmax", "tmin", "RH")) {
  if (!all(predictors %in% names(data))) {
    stop("All predictors must be columns in the data frame.")
  }
  data_for_clustering <- data[, predictors]
  data_scaled <- scale(data_for_clustering)

  set.seed(123) # for reproducibility
  kmeans_result <- kmeans(data_scaled, centers = num_centers, nstart = 25)

  data$cluster <- kmeans_result$cluster
  return(list(data = data, model = kmeans_result))
}

# Hierarchical clustering and heatmap
heatmap_cluster_test <- function(data, file) {
  ds <- dist(data)
  pdf(file)
  heatmap(as.matrix(ds))
  dev.off()
}

#' Assign a new data point to a cluster from a trained k-means model.
#'
#' @param new_point A named vector or data frame with predictor values.
#' @param kmeans_model The trained k-means model object from `kmeans()`.
#' @return The cluster number.
classify_point <- function(new_point, kmeans_model) {
  # Ensure new_point is a data frame and in the correct order
  if (!is.data.frame(new_point)) {
    new_point <- as.data.frame(t(new_point))
  }
  new_point <- new_point[, colnames(kmeans_model$centers)]

  # Scale the new point using the attributes from the original scaling
  scaled_point <- scale(new_point,
                        center = attr(kmeans_model$centers, "scaled:center"),
                        scale = attr(kmeans_model$centers, "scaled:scale"))

  # Calculate Euclidean distance to each cluster center
  distances <- apply(kmeans_model$centers, 1, function(center) {
    sqrt(sum((scaled_point - center)^2))
  })
  return(which.min(distances))
}
