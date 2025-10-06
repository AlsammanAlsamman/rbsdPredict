# Hybrid model functions for RBSD project
library(randomForest)
library(stats)

# Store model coefficients in lists for easier management
# Format: (Intercept), tmax, tmin, rh
a_model_coeffs <- list(
  "1" = c(37.41, -1.07, 0.98, -0.10),
  "2" = c(66.57, -0.95, -0.87, 0.05),
  "3" = c(-24.91, -0.16, 0.62, 0.51)
)

pdi_model_coeffs <- list(
  "1" = c(10.08, 0.05, -1.80, 0.21),
  "2" = c(-32.11, 0.22, 0.49, 0.06),
  "3" = c(21.90, -0.59, 1.13, -0.49)
)

#' Generic prediction function for hybrid models
#'
#' @param cluster The cluster number.
#' @param tmax Maximum temperature.
#' @param tmin Minimum temperature.
#' @param rh Relative humidity.
#' @param model_coeffs A list of coefficient vectors, named by cluster.
#' @return A numeric prediction.
predict_hybrid <- function(cluster, tmax, tmin, rh, model_coeffs) {
  cluster_char <- as.character(cluster)
  if (!cluster_char %in% names(model_coeffs)) {
    stop("Invalid cluster number: ", cluster)
  }
  coeffs <- model_coeffs[[cluster_char]]
  prediction <- coeffs[1] + (coeffs[2] * tmax) + (coeffs[3] * tmin) + (coeffs[4] * rh)
  return(prediction)
}
