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
#' @param ... Additional parameters reserved for future extensions
#' @return A ggplot2 dendrogram plot
#' @examples
#' data(rbsd_data)
#' cluster_result <- cluster_analysis(rbsd_data[, c("tmax", "tmin", "RH")])
#' plot_dendrogram(cluster_result)
#' @export
plot_dendrogram <- function(x, ...) {
  if (!inherits(x, "rbsd_cluster")) {
    stop("Input must be an object of class 'rbsd_cluster'")
  }

  hc <- x$hclust
  n <- length(hc$order)
  merge <- hc$merge
  heights <- hc$height

  leaf_x <- integer(n)
  leaf_x[hc$order] <- seq_len(n)

  node_x <- numeric(nrow(merge))
  seg_rows <- list()
  idx <- 1L

  get_node_x <- function(node_id) {
    if (node_id < 0) {
      leaf_x[-node_id]
    } else {
      node_x[node_id]
    }
  }

  get_node_h <- function(node_id) {
    if (node_id < 0) {
      0
    } else {
      heights[node_id]
    }
  }

  for (i in seq_len(nrow(merge))) {
    left <- merge[i, 1]
    right <- merge[i, 2]
    parent_h <- heights[i]

    left_x <- get_node_x(left)
    right_x <- get_node_x(right)
    left_h <- get_node_h(left)
    right_h <- get_node_h(right)

    node_x[i] <- mean(c(left_x, right_x))

    seg_rows[[idx]] <- data.frame(x = left_x, y = left_h, xend = left_x, yend = parent_h)
    idx <- idx + 1L
    seg_rows[[idx]] <- data.frame(x = right_x, y = right_h, xend = right_x, yend = parent_h)
    idx <- idx + 1L
    seg_rows[[idx]] <- data.frame(x = left_x, y = parent_h, xend = right_x, yend = parent_h)
    idx <- idx + 1L
  }

  seg_df <- do.call(rbind, seg_rows)

  leaf_clusters <- factor(x$clusters[hc$order])
  label_df <- data.frame(
    x = seq_len(n),
    y = 0,
    cluster = leaf_clusters
  )

  cluster_blocks <- split(seq_len(n), label_df$cluster)
  block_df <- do.call(rbind, lapply(names(cluster_blocks), function(cl) {
    block <- cluster_blocks[[cl]]
    data.frame(cluster = cl, xmin = min(block) - 0.5, xmax = max(block) + 0.5)
  }))

  cut_height <- if (x$n_clusters > 1) {
    sort(heights, decreasing = TRUE)[x$n_clusters - 1]
  } else {
    max(heights)
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = block_df,
      ggplot2::aes_string(xmin = "xmin", xmax = "xmax", fill = "cluster"),
      ymin = 0,
      ymax = cut_height,
      alpha = 0.08,
      color = NA
    ) +
    ggplot2::geom_segment(
      data = seg_df,
      ggplot2::aes_string(x = "x", y = "y", xend = "xend", yend = "yend"),
      linewidth = 0.35,
      color = "#2f3b52"
    ) +
    ggplot2::geom_point(
      data = label_df,
      ggplot2::aes_string(x = "x", y = "y", color = "cluster"),
      size = 1.2,
      alpha = 0.9
    ) +
    ggplot2::labs(
      title = "Hierarchical Clustering Dendrogram",
      subtitle = paste("Method:", x$method, "| Clusters:", x$n_clusters),
      x = "Samples (ordered leaves)",
      y = "Distance"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "#4a5568")
    )

  return(p)
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
  p <- ggplot2::ggplot(melted_data, ggplot2::aes_string(x = "variable", y = "value", fill = "variable")) +
    ggplot2::geom_boxplot(width = 0.72, alpha = 0.9, outlier.alpha = 0.35) +
    ggplot2::facet_wrap(~cluster) +
    ggplot2::labs(title = "Boxplots of Variables by Cluster",
                  x = "Variable", y = "Value") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none"
    )
  
  return(p)
}
