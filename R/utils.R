# Utility functions for RBSD project
library(Metrics)

# RMSE calculation
rmse <- function(actual, predicted) {
  Metrics::rmse(actual, predicted)
}

# Accuracy calculation
accuracy <- function(actual, predicted) {
  1 - Metrics::rmse(actual, predicted) / mean(actual)
}

#' Load RBSD dataset from Data.tsv and make it available via data() for package use
#'
#' The dataset is from:
#' Modelling and Scaling Weather-Driven Epidemiology of Rice Brown Spot Disease in Peninsular India
#' Sudhasha Selvaraj et al.
#' See package documentation for full citation and details.
#'
#' @examples
#' data(rbsd_data)
#' head(rbsd_data)
#' summary(rbsd_data)
#'
rbsd_data <- read.table(
  file = "outFolder/Data.tsv",
  header = TRUE,
  sep = "\t",
  row.names = 1
)

# Make available via data() for package users
data <- function() {
  rbsd_data
}
