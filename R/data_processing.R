
#' Preprocess Data for RBSD Prediction
#'
#' This function handles missing values, normalizes features, and prepares
#' data for modeling.
#'
#' @param data A data frame containing environmental variables and disease indicators
#' @param handle_missing Method to handle missing values ("impute" or "remove")
#' @param normalize Whether to normalize features (default: TRUE)
#' @param seed Random seed for reproducibility (default: 123)
#' @return A list containing processed data and preprocessing parameters
#' @examples
#' data(rbsd_data)
#' processed <- preprocess_data(rbsd_data)
#' @export
preprocess_data <- function(data, handle_missing = "impute", normalize = TRUE, seed = 123) {
  
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
    # Identify numeric columns (excluding potential target variables)
    numeric_cols <- sapply(processed_data, is.numeric)
    
    # Calculate normalization parameters
    prep_params$means <- colMeans(processed_data[, numeric_cols], na.rm = TRUE)
    prep_params$sds <- apply(processed_data[, numeric_cols], 2, sd, na.rm = TRUE)
    
    # Apply normalization
    processed_data[, numeric_cols] <- scale(processed_data[, numeric_cols])
  }
  
  # Split data into features and targets
  feature_cols <- c("tmax", "tmin", "RH", "RF", "PET", "WS")
  target_cols <- c("A", "PDI")
  
  features <- processed_data[, intersect(feature_cols, colnames(processed_data)), drop = FALSE]
  targets <- processed_data[, intersect(target_cols, colnames(processed_data)), drop = FALSE]
  
  # Create train-test split
  train_indices <- sample(1:nrow(processed_data), size = floor(0.8 * nrow(processed_data)))
  
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
    handle_missing = handle_missing
  )
  
  class(result) <- "rbsd_processed"
  
  return(result)
}