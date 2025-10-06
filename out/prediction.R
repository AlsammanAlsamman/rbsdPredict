# Prediction functions for RBSD project
source("clustering.R")
source("hybrid_model.R")

# Predict cluster for each row
data_predict_clusters <- function(data) {
  data$predicted_cluster <- apply(
    data[, c("tmax", "tmin", "RH")],
    1,
    function(x) classify_point(x[1], x[2], x[3])
  )
  return(data)
}

# Predict A and PDI for each row using hybrid model
predict_a_pdi <- function(data) {
  data$predicted_a <- mapply(
    predict_a,
    data$predicted_cluster,
    data$tmax,
    data$tmin,
    data$rh
  )
  data$predicted_pdi <- mapply(
    predict_pdi,
    data$predicted_cluster,
    data$tmax,
    data$tmin,
    data$rh
  )
  data
}

# Calculate RMSE for predictions
calc_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}
