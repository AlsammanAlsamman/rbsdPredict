#' Train Machine Learning Models for RBSD Prediction
#'
#' This function trains multiple machine learning models to predict RBSD
#' indicators (A and PDI) based on environmental variables.
#'
#' @param processed_data Processed data from preprocess_data()
#' @param models Character vector of models to train (default: all available)
#' @param tune Whether to perform hyperparameter tuning (default: FALSE)
#' @param seed Random seed for reproducibility (default: 123)
#' @return A list containing trained models and evaluation metrics
#' @examples
#' data(rbsd_data)
#' processed <- preprocess_data(rbsd_data)
#' ml_models <- train_ml_models(processed)
#' @export
train_ml_models <- function(processed_data, models = NULL, tune = FALSE, seed = 123) {
  
  if (!inherits(processed_data, "rbsd_processed")) {
    stop("Input must be an object of class 'rbsd_processed'")
  }
  
  set.seed(seed)
  
  # Default to all available models
  if (is.null(models)) {
    models <- c("lm", "svm", "rf", "xgbTree", "knn", "nnet")
  }
  
  # Prepare data for caret
  train_data <- cbind(processed_data$train_features, processed_data$train_targets)
  test_data <- cbind(processed_data$test_features, processed_data$test_targets)
  
  # Train models for each target variable
  results <- list()
  
  for (target in c("A", "PDI")) {
    if (target %in% colnames(processed_data$train_targets)) {
      target_results <- list()
      
      # Define model configurations
      model_configs <- list(
        lm = caret::trainControl(method = "cv", number = 5),
        svm = caret::trainControl(method = "cv", number = 5),
        rf = caret::trainControl(method = "cv", number = 5),
        xgbTree = caret::trainControl(method = "cv", number = 5),
        knn = caret::trainControl(method = "cv", number = 5),
        nnet = caret::trainControl(method = "cv", number = 5)
      )
      
      # Train each model
      for (model in models) {
        if (model %in% names(model_configs)) {
          message(paste("Training", model, "for target:", target))
          
          # Train model
          model_fit <- caret::train(
            as.formula(paste(target, "~ .")),
            data = train_data,
            method = model,
            trControl = model_configs[[model]]
          )
          
          # Make predictions
          predictions <- predict(model_fit, newdata = test_data)
          
          # Calculate metrics
          rmse <- Metrics::rmse(test_data[[target]], predictions)
          r2 <- cor(test_data[[target]], predictions)^2
          
          # Store results
          target_results[[model]] <- list(
            model = model_fit,
            predictions = predictions,
            rmse = rmse,
            r2 = r2
          )
        }
      }
      
      results[[target]] <- target_results
    }
  }
  
  class(results) <- "rbsd_ml_models"
  
  return(results)
}

#' Evaluate Machine Learning Models
#'
#' @param ml_models Trained models from train_ml_models()
#' @return Performance metrics and visualizations
#' @examples
#' data(rbsd_data)
#' processed <- preprocess_data(rbsd_data)
#' ml_models <- train_ml_models(processed)
#' evaluation <- evaluate_model(ml_models)
#' @export
evaluate_model <- function(ml_models) {
  
  if (!inherits(ml_models, "rbsd_ml_models")) {
    stop("Input must be an object of class 'rbsd_ml_models'")
  }
  
  # Extract test data
  test_data <- cbind(ml_models$test_features, ml_models$test_targets)
  
  # Create results data frame
  results_df <- data.frame()
  
  # Collect metrics for each model and target
  for (target in names(ml_models)) {
    if (target %in% c("A", "PDI")) {
      for (model in names(ml_models[[target]])) {
        model_info <- ml_models[[target]][[model]]
        
        results_df <- rbind(results_df, data.frame(
          Target = target,
          Model = model,
          RMSE = model_info$rmse,
          R2 = model_info$r2
        ))
      }
    }
  }
  
  # Create performance visualization
  p1 <- ggplot2::ggplot(results_df, ggplot2::aes(x = Model, y = RMSE, fill = Target)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(title = "RMSE by Model and Target",
                 x = "Model", y = "RMSE") +
    ggplot2::theme_minimal()
  
  p2 <- ggplot2::ggplot(results_df, ggplot2::aes(x = Model, y = R2, fill = Target)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(title = "R-squared by Model and Target",
                 x = "Model", y = "R-squared") +
    ggplot2::theme_minimal()
  
  # Return results
  result <- list(
    metrics = results_df,
    rmse_plot = p1,
    r2_plot = p2
  )
  
  class(result) <- "rbsd_evaluation"
  
  return(result)
}
