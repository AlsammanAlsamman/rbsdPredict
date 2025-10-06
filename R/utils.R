#' Print Method for rbsd_cluster Objects
#'
#' @param x An object of class "rbsd_cluster"
#' @param ... Additional parameters passed to print()
#' @export
print.rbsd_cluster <- function(x, ...) {
  cat("Hierarchical Clustering Analysis\n")
  cat("================================\n")
  cat("Number of clusters:", x$n_clusters, "\n")
  cat("Clustering method:", x$method, "\n")
  cat("Cluster distribution:\n")
  print(table(x$clusters))
  cat("\n")
}

#' Print Method for rbsd_processed Objects
#'
#' @param x An object of class "rbsd_processed"
#' @param ... Additional parameters passed to print()
#' @export
print.rbsd_processed <- function(x, ...) {
  cat("Processed RBSD Data\n")
  cat("===================\n")
  cat("Training samples:", nrow(x$train_features), "\n")
  cat("Test samples:", nrow(x$test_features), "\n")
  cat("Features:", ncol(x$train_features), "\n")
  cat("Targets:", ncol(x$train_targets), "\n")
  cat("Missing values handled:", x$handle_missing, "\n")
  cat("Features normalized:", x$normalize, "\n")
  cat("\n")
}

#' Print Method for rbsd_ml_models Objects
#'
#' @param x An object of class "rbsd_ml_models"
#' @param ... Additional parameters passed to print()
#' @export
print.rbsd_ml_models <- function(x, ...) {
  cat("Machine Learning Models for RBSD Prediction\n")
  cat("==========================================\n")
  
  for (target in names(x)) {
    if (target %in% c("A", "PDI")) {
      cat("Target:", target, "\n")
      cat("Models trained:", length(x[[target]]), "\n")
      
      # Find best model
      model_metrics <- sapply(x[[target]], function(m) m$rmse)
      best_model <- names(which.min(model_metrics))
      best_rmse <- min(model_metrics)
      
      cat("Best model:", best_model, "(RMSE =", round(best_rmse, 4), ")\n")
      cat("\n")
    }
  }
}

#' Print Method for rbsd_hybrid_model Objects
#'
#' @param x An object of class "rbsd_hybrid_model"
#' @param ... Additional parameters passed to print()
#' @export
print.rbsd_hybrid_model <- function(x, ...) {
  cat("Hybrid Model for RBSD Prediction\n")
  cat("===============================\n")
  cat("Number of clusters:", x$cluster_result$n_clusters, "\n")
  cat("Clustering method:", x$cluster_result$method, "\n")
  
  cat("\nModel Performance:\n")
  print(x$metrics)
  cat("\n")
}

#' Print Method for rbsd_evaluation Objects
#'
#' @param x An object of class "rbsd_evaluation"
#' @param ... Additional parameters passed to print()
#' @export
print.rbsd_evaluation <- function(x, ...) {
  cat("Model Evaluation Results\n")
  cat("======================\n")
  print(x$metrics)
  cat("\n")
}
