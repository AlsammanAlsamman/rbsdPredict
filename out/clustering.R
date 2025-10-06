#' Perform Hierarchical Clustering Analysis
#'
#' This function performs hierarchical clustering on environmental variables
#' to identify distinct climate scenarios that may influence disease development.
#'
#' @param data A data frame containing environmental variables (tmax, tmin, RH, etc.)
#' @param n_clusters Number of clusters to create (default: 3)
#' @param method Clustering method (default: "ward.D2")
#' @return A list containing clustering results and visualizations
#' @examples
#' data(rbsd_data)
#' cluster_result <- cluster_analysis(rbsd_data[, c("tmax", "tmin", "RH")])
#' @export
cluster_analysis <- function(data, n_clusters = 3, method = "ward.D2") {
  
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  required_cols <- c("tmax", "tmin", "RH")
  if (!all(required_cols %in% colnames(data))) {
    stop(paste("Data must contain columns:", paste(required_cols, collapse = ", ")))
  }
  
  # Scale the data
  scaled_data <- scale(data)
  
  # Calculate distance matrix
  dist_matrix <- dist(scaled_data, method = "euclidean")
  
  # Perform hierarchical clustering
  hc <- hclust(dist_matrix, method = method)
  
  # Cut tree to get clusters
  clusters <- cutree(hc, k = n_clusters)
  
  # Create result object
  result <- list(
    hclust = hc,
    clusters = clusters,
    scaled_data = scaled_data,
    n_clusters = n_clusters,
    method = method
  )
  
  class(result) <- "rbsd_cluster"
  
  return(result)
}

#' Plot Dendrogram from Clustering Analysis
#'
#' @param x An object of class "rbsd_cluster" from cluster_analysis()
#' @param ... Additional parameters passed to plot()
#' @return A dendrogram plot
#' @examples
#' data(rbsd_data)
#' cluster_result <- cluster_analysis(rbsd_data[, c("tmax", "tmin", "RH")])
#' plot_dendrogram(cluster_result)
#' @export
plot_dendrogram <- function(x, ...) {
  if (!inherits(x, "rbsd_cluster")) {
    stop("Input must be an object of class 'rbsd_cluster'")
  }
  
  plot(x$hclust, main = "Hierarchical Clustering Dendrogram", 
       xlab = "Samples", ylab = "Distance", ...)
  rect.hclust(x$hclust, k = x$n_clusters, border = "red")
}

#' Plot Cluster Boxplots
#'
#' @param x An object of class "rbsd_cluster" from cluster_analysis()
#' @param data Original data frame used for clustering
#' @return Boxplots showing variable distribution by cluster
#' @examples
#' data(rbsd_data)
#' cluster_result <- cluster_analysis(rbsd_data[, c("tmax", "tmin", "RH")])
#' plot_cluster_boxplots(cluster_result, rbsd_data[, c("tmax", "tmin", "RH")])
#' @export
plot_cluster_boxplots <- function(x, data) {
  if (!inherits(x, "rbsd_cluster")) {
    stop("Input must be an object of class 'rbsd_cluster'")
  }
  
  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }
  
  # Add cluster information to data
  data_with_clusters <- data.frame(data, cluster = factor(x$clusters))
  
  # Melt data for ggplot
  melted_data <- reshape2::melt(data_with_clusters, id.vars = "cluster")
  
  # Create boxplot
  p <- ggplot2::ggplot(melted_data, ggplot2::aes(x = variable, y = value, fill = variable)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~cluster) +
    ggplot2::labs(title = "Boxplots of Variables by Cluster",
                  x = "Variable", y = "Value") +
    ggplot2::theme_minimal()
  
  return(p)
}

