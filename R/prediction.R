#' Predict RBSD Using Trained Models
#'
#' This function makes predictions using either machine learning models
#' or the hybrid model.
#'
#' @param model Trained model from train_ml_models() or train_hybrid_model()
#' @param newdata New data for prediction
#' @param type Type of prediction ("A" or "PDI")
#' @return Predictions for the specified target
#' @examples
#' data(rbsd_data)
#' processed <- preprocess_data(rbsd_data)
#' ml_models <- train_ml_models(processed)
#' predictions <- predict_rbsd(ml_models, processed$test_features, "A")
#' @export
predict_rbsd <- function(model, newdata, type = c("A", "PDI")) {
  
  type <- match.arg(type)
  
  if (inherits(model, "rbsd_ml_models")) {
    # Prediction with ML models
    if (!is.data.frame(newdata)) {
      stop("newdata must be a data frame")
    }
    
    # Check if we have a model for this target
    if (!type %in% names(model)) {
      stop(paste("No model available for target:", type))
    }
    
    # Use the best model (lowest RMSE) for predictions
    model_metrics <- sapply(model[[type]], function(x) x$rmse)
    best_model <- names(which.min(model_metrics))
    
    message(paste("Using", best_model, "for predictions of", type))
    
    predictions <- predict(model[[type]][[best_model]]$model, newdata = newdata)
    
  } else if (inherits(model, "rbsd_hybrid_model")) {
    # Prediction with hybrid model
    if (!is.data.frame(newdata)) {
      stop("newdata must be a data frame")
    }
    
    required_cols <- c("tmax", "tmin", "RH")
    if (!all(required_cols %in% colnames(newdata))) {
      stop(paste("newdata must contain columns:", paste(required_cols, collapse = ", ")))
    }
    
    if (type == "A") {
      predictions <- mapply(model$predict_A, newdata$tmax, newdata$tmin, newdata$RH)
    } else {
      predictions <- mapply(model$predict_PDI, newdata$tmax, newdata$tmin, newdata$RH)
    }
    
  } else {
    stop("model must be of class 'rbsd_ml_models' or 'rbsd_hybrid_model'")
  }
  
  return(predictions)
}

#' Plot Actual vs Predicted Values
#'
#' @param actual Actual values
#' @param predicted Predicted values
#' @param target Target variable name ("A" or "PDI")
#' @return A scatter plot of actual vs predicted values
#' @examples
#' data(rbsd_data)
#' processed <- preprocess_data(rbsd_data)
#' ml_models <- train_ml_models(processed)
#' predictions <- predict_rbsd(ml_models, processed$test_features, "A")
#' plot_actual_vs_predicted(processed$test_targets$A, predictions, "A")
#' @export
plot_actual_vs_predicted <- function(actual, predicted, target = "Target") {
  
  if (length(actual) != length(predicted)) {
    stop("actual and predicted must have the same length")
  }
  
  # Remove NA values
  complete_cases <- complete.cases(actual, predicted)
  actual <- actual[complete_cases]
  predicted <- predicted[complete_cases]
  
  # Create data frame for plotting
  plot_data <- data.frame(
    Actual = actual,
    Predicted = predicted
  )
  
  # Calculate R-squared
  r2 <- cor(actual, predicted)^2
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Actual, y = Predicted)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    ggplot2::labs(title = paste("Actual vs Predicted", target),
                 x = "Actual Values", y = "Predicted Values") +
    ggplot2::annotate("text", x = min(actual), y = max(predicted), 
                      label = paste("R² =", round(r2, 3)), hjust = 0, vjust = 1) +
    ggplot2::theme_minimal()
  
  return(p)
}
