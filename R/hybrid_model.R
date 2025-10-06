#' Train Hybrid Model for RBSD Prediction
#'
#' This function implements a hybrid approach that combines clustering
#' with regression models for improved prediction accuracy.
#'
#' @param data A data frame containing environmental variables and disease indicators
#' @param n_clusters Number of clusters for the hybrid model (default: 3)
#' @param seed Random seed for reproducibility (default: 123)
#' @return A list containing the hybrid model and evaluation metrics
#' @examples
#' data(rbsd_data)
#' hybrid_model <- train_hybrid_model(rbsd_data)
#' @export
train_hybrid_model <- function(data, n_clusters = 3, seed = 123) {
  
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  set.seed(seed)
  
  # Check for required columns
  required_cols <- c("tmax", "tmin", "RH", "A", "PDI")
  missing_cols <- setdiff(required_cols, colnames(data))
  
  if (length(missing_cols) > 0) {
    stop(paste("Data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Step 1: Perform clustering
  message("Performing hierarchical clustering...")
  cluster_data <- data[, c("tmax", "tmin", "RH")]
  cluster_result <- cluster_analysis(cluster_data, n_clusters = n_clusters)
  
  # Add cluster information to data
  data_with_clusters <- data.frame(data, cluster = cluster_result$clusters)
  
  # Step 2: Derive cluster rules
  message("Deriving cluster rules...")
  cluster_rules <- derive_cluster_rules(data_with_clusters, "cluster", c("tmax", "tmin", "RH"))
  
  # Step 3: Train regression models for each cluster
  message("Training cluster-specific regression models...")
  models_A <- list()
  models_PDI <- list()
  
  for (i in 1:n_clusters) {
    cluster_data <- data_with_clusters[data_with_clusters$cluster == i, ]
    
    if (nrow(cluster_data) > 0) {
      # Train model for A
      model_A <- stats::lm(A ~ tmax + tmin + RH, data = cluster_data)
      models_A[[as.character(i)]] <- model_A
      
      # Train model for PDI
      model_PDI <- stats::lm(PDI ~ tmax + tmin + RH, data = cluster_data)
      models_PDI[[as.character(i)]] <- model_PDI
    }
  }
  
  # Step 4: Create prediction functions
  predict_A <- function(tmax, tmin, RH) {
    cluster <- predict_cluster_from_rules(tmax, tmin, RH, cluster_rules)
    
    if (is.na(cluster) || !(cluster %in% names(models_A))) {
      return(NA)
    }
    
    model <- models_A[[as.character(cluster)]]
    new_data <- data.frame(tmax = tmax, tmin = tmin, RH = RH)
    return(predict(model, newdata = new_data))
  }
  
  predict_PDI <- function(tmax, tmin, RH) {
    cluster <- predict_cluster_from_rules(tmax, tmin, RH, cluster_rules)
    
    if (is.na(cluster) || !(cluster %in% names(models_PDI))) {
      return(NA)
    }
    
    model <- models_PDI[[as.character(cluster)]]
    new_data <- data.frame(tmax = tmax, tmin = tmin, RH = RH)
    return(predict(model, newdata = new_data))
  }
  
  # Step 5: Evaluate the hybrid model
  message("Evaluating hybrid model...")
  data_with_clusters$predicted_A <- mapply(predict_A, data_with_clusters$tmax, 
                                           data_with_clusters$tmin, data_with_clusters$RH)
  data_with_clusters$predicted_PDI <- mapply(predict_PDI, data_with_clusters$tmax, 
                                             data_with_clusters$tmin, data_with_clusters$RH)
  
  # Calculate metrics
  rmse_A <- Metrics::rmse(data_with_clusters$A, data_with_clusters$predicted_A, na.rm = TRUE)
  rmse_PDI <- Metrics::rmse(data_with_clusters$PDI, data_with_clusters$predicted_PDI, na.rm = TRUE)
  
  r2_A <- cor(data_with_clusters$A, data_with_clusters$predicted_A, use = "complete.obs")^2
  r2_PDI <- cor(data_with_clusters$PDI, data_with_clusters$predicted_PDI, use = "complete.obs")^2
  
  # Return results
  result <- list(
    cluster_result = cluster_result,
    cluster_rules = cluster_rules,
    models_A = models_A,
    models_PDI = models_PDI,
    predict_A = predict_A,
    predict_PDI = predict_PDI,
    predictions = data_with_clusters[, c("A", "PDI", "predicted_A", "predicted_PDI")],
    metrics = data.frame(
      Target = c("A", "PDI"),
      RMSE = c(rmse_A, rmse_PDI),
      R2 = c(r2_A, r2_PDI)
    )
  )
  
  class(result) <- "rbsd_hybrid_model"
  
  return(result)
}

#' Derive Cluster Rules from Data
#'
#' @param data Data frame with cluster assignments
#' @param cluster_col Name of column containing cluster assignments
#' @param feature_cols Names of feature columns
#' @return List of cluster rules
#' @keywords internal
derive_cluster_rules <- function(data, cluster_col, feature_cols) {
  rules <- list()
  
  for (cluster_id in sort(unique(data[[cluster_col]]))) {
    cluster_data <- data[data[[cluster_col]] == cluster_id, feature_cols, drop = FALSE]
    
    if (nrow(cluster_data) > 0) {
      min_vals <- sapply(cluster_data, min, na.rm = TRUE)
      max_vals <- sapply(cluster_data, max, na.rm = TRUE)
      
      rules[[as.character(cluster_id)]] <- list(
        min = min_vals,
        max = max_vals
      )
    }
  }
  
  return(rules)
}

#' Predict Cluster from Rules
#'
#' @param tmax Maximum temperature
#' @param tmin Minimum temperature
#' @param RH Relative humidity
#' @param rules Cluster rules from derive_cluster_rules()
#' @return Predicted cluster ID
#' @keywords internal
predict_cluster_from_rules <- function(tmax, tmin, RH, rules) {
  for (cluster_id in names(rules)) {
    rule <- rules[[cluster_id]]
    
    if (tmax >= rule$min["tmax"] && tmax <= rule$max["tmax"] &&
        tmin >= rule$min["tmin"] && tmin <= rule$max["tmin"] &&
        RH >= rule$min["RH"] && RH <= rule$max["RH"]) {
      return(as.numeric(cluster_id))
    }
  }
  
  return(NA)
}
