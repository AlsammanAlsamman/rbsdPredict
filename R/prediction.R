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
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))

  lower <- min(c(actual, predicted))
  upper <- max(c(actual, predicted))
  ann <- paste0("R² = ", round(r2, 3), "\nRMSE = ", round(rmse, 3), "\nMAE = ", round(mae, 3))
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "Actual", y = "Predicted")) +
    ggplot2::geom_point(color = "#2b6cb0", alpha = 0.55, size = 2) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "#2f855a", linewidth = 0.9) +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "#c53030", linetype = "dashed", linewidth = 0.8) +
    ggplot2::coord_equal(xlim = c(lower, upper), ylim = c(lower, upper)) +
    ggplot2::labs(
      title = paste("Actual vs Predicted", target),
      subtitle = "Dashed red line: perfect agreement | Green line: fitted trend",
      x = "Actual Values",
      y = "Predicted Values"
    ) +
    ggplot2::annotate(
      "label",
      x = lower + 0.03 * (upper - lower),
      y = upper - 0.03 * (upper - lower),
      label = ann,
      hjust = 0,
      vjust = 1,
      size = 3.4,
      fill = "white"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "#4a5568")
    )
  
  return(p)
}
