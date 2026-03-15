#' Plot Risk Map for RBSD
#'
#' This function creates a visualization of RBSD risk based on environmental conditions.
#'
#' @param data Data frame containing environmental variables
#' @param model Trained model for prediction
#' @param type Type of prediction ("A" or "PDI")
#' @param resolution Resolution of the risk map (default: 50)
#' @return A risk map visualization
#' @examples
#' data(rbsd_data)
#' processed <- preprocess_data(rbsd_data)
#' ml_models <- train_ml_models(processed)
#' plot_risk_map(rbsd_data[, c("tmax", "tmin", "RH")], ml_models, "A")
#' @export
plot_risk_map <- function(data, model, type = c("A", "PDI"), resolution = 120) {
  
  type <- match.arg(type)
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  required_cols <- c("tmax", "tmin", "RH")
  if (!all(required_cols %in% colnames(data))) {
    stop(paste("data must contain columns:", paste(required_cols, collapse = ", ")))
  }
  
  # Create a grid of values for prediction
  tmax_range <- seq(min(data$tmax), max(data$tmax), length.out = resolution)
  rh_range <- seq(min(data$RH), max(data$RH), length.out = resolution)
  
  # Use mean tmin for all predictions
  mean_tmin <- mean(data$tmin)
  
  # Create grid
  grid <- expand.grid(tmax = tmax_range, tmin = mean_tmin, RH = rh_range)
  
  # Make predictions
  grid$prediction <- predict_rbsd(model, grid, type)

  if (all(is.na(grid$prediction))) {
    stop("All predictions are NA. Check model compatibility and input feature ranges.")
  }
  
  # Create plot
  p <- ggplot2::ggplot(grid, ggplot2::aes_string(x = "tmax", y = "RH", fill = "prediction")) +
    ggplot2::geom_raster(interpolate = TRUE) +
    ggplot2::geom_contour(
      ggplot2::aes_string(x = "tmax", y = "RH", z = "prediction"),
      inherit.aes = FALSE,
      bins = 8,
      color = "white",
      linewidth = 0.28,
      alpha = 0.75
    ) +
    ggplot2::scale_fill_viridis_c(option = "magma", na.value = "grey90") +
    ggplot2::labs(
      title = paste("RBSD Risk Map for", type),
      subtitle = "Surface at mean tmin with contour overlays",
      x = "Maximum Temperature (°C)",
      y = "Relative Humidity (%)",
      fill = paste("Predicted", type)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "#4a5568")
    )
  
  return(p)
}
