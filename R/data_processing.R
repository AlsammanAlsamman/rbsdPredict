
#' Preprocess Data for RBSD Prediction
#'
#' This function handles missing values, normalizes features, and prepares
#' data for modeling.
#'
#' @param data A data frame containing environmental variables and disease indicators
#' @param handle_missing Method to handle missing values ("impute" or "remove")
#' @param normalize Whether to normalize features (default: TRUE)
#' @param normalize_targets Whether to normalize target columns A and PDI (default: FALSE)
#' @param seed Random seed for reproducibility (default: 123)
#' @return A list containing processed data and preprocessing parameters
#' @examples
#' data(rbsd_data)
#' processed <- preprocess_data(rbsd_data)
#' @export
preprocess_data <- function(data, handle_missing = "impute", normalize = TRUE,
                            normalize_targets = FALSE, seed = 123) {
  
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  set.seed(seed)
  
  # Make a copy to avoid modifying original data
  processed_data <- data
  
  # Handle missing values
  if (any(is.na(processed_data))) {
    if (handle_missing == "impute") {
      # Impute missing values with column means
      for (col in colnames(processed_data)) {
        if (is.numeric(processed_data[[col]])) {
          processed_data[[col]][is.na(processed_data[[col]])] <- mean(processed_data[[col]], na.rm = TRUE)
        }
      }
    } else if (handle_missing == "remove") {
      processed_data <- na.omit(processed_data)
    } else {
      stop("handle_missing must be either 'impute' or 'remove'")
    }
  }
  
  # Store preprocessing parameters
  prep_params <- list()
  
  # Normalize features if requested
  if (normalize) {
    feature_cols <- c("tmax", "tmin", "RH", "RF", "PET", "WS")
    target_cols <- c("A", "PDI")
    columns_to_scale <- intersect(feature_cols, colnames(processed_data))

    # Fallback for generic tabular data.
    if (length(columns_to_scale) == 0) {
      columns_to_scale <- names(processed_data)[sapply(processed_data, is.numeric)]
    }

    if (normalize_targets) {
      columns_to_scale <- unique(c(columns_to_scale, intersect(target_cols, colnames(processed_data))))
    }
    
    # Calculate normalization parameters
    prep_params$scaled_columns <- columns_to_scale
    prep_params$means <- colMeans(processed_data[, columns_to_scale, drop = FALSE], na.rm = TRUE)
    prep_params$sds <- apply(processed_data[, columns_to_scale, drop = FALSE], 2, sd, na.rm = TRUE)
    
    # Apply normalization
    processed_data[, columns_to_scale] <- scale(processed_data[, columns_to_scale, drop = FALSE])
  }
  
  # Split data into features and targets
  feature_cols <- c("tmax", "tmin", "RH", "RF", "PET", "WS")
  target_cols <- c("A", "PDI")
  
  features <- processed_data[, intersect(feature_cols, colnames(processed_data)), drop = FALSE]
  targets <- processed_data[, intersect(target_cols, colnames(processed_data)), drop = FALSE]
  
  # Create train-test split
  train_indices <- sample(seq_len(nrow(processed_data)), size = floor(0.8 * nrow(processed_data)))
  
  train_features <- features[train_indices, , drop = FALSE]
  test_features <- features[-train_indices, , drop = FALSE]
  train_targets <- targets[train_indices, , drop = FALSE]
  test_targets <- targets[-train_indices, , drop = FALSE]
  
  # Return processed data and parameters
  result <- list(
    train_features = train_features,
    test_features = test_features,
    train_targets = train_targets,
    test_targets = test_targets,
    prep_params = prep_params,
    normalize = normalize,
    normalize_targets = normalize_targets,
    handle_missing = handle_missing
  )
  
  class(result) <- "rbsd_processed"
  
  return(result)
}