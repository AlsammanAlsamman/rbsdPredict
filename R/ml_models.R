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
  
  # Train models for each target variable
  results <- list()

  safe_metrics <- function(actual, predicted) {
    keep <- stats::complete.cases(actual, predicted)
    actual <- actual[keep]
    predicted <- predicted[keep]

    if (length(actual) == 0) {
      return(list(rmse = NA_real_, r2 = NA_real_, mae = NA_real_))
    }

    rmse <- Metrics::rmse(actual, predicted)
    mae <- Metrics::mae(actual, predicted)
    r2 <- if (stats::sd(actual) == 0 || stats::sd(predicted) == 0) {
      NA_real_
    } else {
      stats::cor(actual, predicted)^2
    }

    list(rmse = rmse, r2 = r2, mae = mae)
  }
  
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

          train_data_target <- processed_data$train_features
          train_data_target[[target]] <- processed_data$train_targets[[target]]
          
          # Train model
          model_fit <- caret::train(
            as.formula(paste(target, "~ .")),
            data = train_data_target,
            method = model,
            trControl = model_configs[[model]]
          )
          
          # Make predictions
          predictions <- predict(model_fit, newdata = processed_data$test_features)
          
          # Calculate metrics
          metrics <- safe_metrics(processed_data$test_targets[[target]], predictions)
          
          # Store results
          target_results[[model]] <- list(
            model = model_fit,
            predictions = predictions,
            rmse = metrics$rmse,
            r2 = metrics$r2,
            mae = metrics$mae
          )
        }
      }
      
      results[[target]] <- target_results
    }
  }

  results$train_features <- processed_data$train_features
  results$test_features <- processed_data$test_features
  results$train_targets <- processed_data$train_targets
  results$test_targets <- processed_data$test_targets
  results$models_requested <- models
  results$tune <- tune
  results$seed <- seed
  
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
  
  # Create results data frame
  results_df <- data.frame()
  
  # Collect metrics for each model and target
  for (target in intersect(c("A", "PDI"), names(ml_models))) {
    for (model in names(ml_models[[target]])) {
      model_info <- ml_models[[target]][[model]]

      results_df <- rbind(results_df, data.frame(
        Target = target,
        Model = model,
        RMSE = model_info$rmse,
        R2 = model_info$r2,
        MAE = model_info$mae
      ))
    }
  }

  if (nrow(results_df) == 0) {
    stop("No trained models were found in the supplied object")
  }

  # Create performance visualization
  p1 <- ggplot2::ggplot(results_df, ggplot2::aes_string(x = "Model", y = "RMSE", fill = "Target")) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(title = "RMSE by Model and Target",
                 x = "Model", y = "RMSE") +
    ggplot2::theme_minimal()

  p2 <- ggplot2::ggplot(results_df, ggplot2::aes_string(x = "Model", y = "R2", fill = "Target")) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(title = "R-squared by Model and Target",
                 x = "Model", y = "R-squared") +
    ggplot2::theme_minimal()

  p3 <- ggplot2::ggplot(results_df, ggplot2::aes_string(x = "Model", y = "MAE", fill = "Target")) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(title = "MAE by Model and Target",
                 x = "Model", y = "MAE") +
    ggplot2::theme_minimal()

  # Return results
  result <- list(
    metrics = results_df,
    rmse_plot = p1,
    r2_plot = p2,
    mae_plot = p3
  )

  class(result) <- "rbsd_evaluation"

  return(result)
}

#' Rank Trained Models by Metric
#'
#' Creates a target-wise leaderboard from an object returned by
#' train_ml_models().
#'
#' @param ml_models Trained models from train_ml_models()
#' @param metric Metric used for ranking ("RMSE", "R2", or "MAE")
#' @return A data frame sorted by target and chosen metric
#' @examples
#' data(rbsd_data)
#' processed <- preprocess_data(rbsd_data)
#' ml_models <- train_ml_models(processed, models = c("lm", "rf"))
#' rank_models(ml_models, metric = "RMSE")
#' @export
rank_models <- function(ml_models, metric = c("RMSE", "R2", "MAE")) {
  if (!inherits(ml_models, "rbsd_ml_models")) {
    stop("Input must be an object of class 'rbsd_ml_models'")
  }

  metric <- match.arg(metric)
  evaluation <- evaluate_model(ml_models)
  metrics_df <- evaluation$metrics

  ranking_column <- switch(metric,
                           RMSE = "RMSE",
                           R2 = "R2",
                           MAE = "MAE")

  ranked <- do.call(rbind, lapply(split(metrics_df, metrics_df$Target), function(df) {
    if (metric == "R2") {
      df <- df[order(-df[[ranking_column]]), ]
    } else {
      df <- df[order(df[[ranking_column]]), ]
    }
    df$Rank <- seq_len(nrow(df))
    df
  }))

  rownames(ranked) <- NULL
  ranked[, c("Target", "Rank", "Model", "RMSE", "R2", "MAE")]
}
