# Hybrid model functions for RBSD project
library(randomForest)
library(stats)

# Example: Hybrid prediction using cluster-specific regression equations
predict_a_cluster1 <- function(tmax, tmin, rh) {
  37.41 + (-1.07 * tmax) + (0.98 * tmin) + (-0.1 * rh)
}
predict_pdi_cluster1 <- function(tmax, tmin, rh) {
  10.08 + (0.05 * tmax) + (-1.8 * tmin) + (0.21 * rh)
}
predict_a_cluster2 <- function(tmax, tmin, rh) {
  66.57 + (-0.95 * tmax) + (-0.87 * tmin) + (0.05 * rh)
}
predict_pdi_cluster2 <- function(tmax, tmin, rh) {
  -32.11 + (0.22 * tmax) + (0.49 * tmin) + (0.06 * rh)
}
predict_a_cluster3 <- function(tmax, tmin, rh) {
  -24.91 + (-0.16 * tmax) + (0.62 * tmin) + (0.51 * rh)
}
predict_pdi_cluster3 <- function(tmax, tmin, rh) {
  21.9 + (-0.59 * tmax) + (1.13 * tmin) + (-0.49 * rh)
}

# Select prediction function by cluster
predict_a <- function(cluster, tmax, tmin, rh) {
  if (cluster == 1) predict_a_cluster1(tmax, tmin, rh)
  else if (cluster == 2) predict_a_cluster2(tmax, tmin, rh)
  else if (cluster == 3) predict_a_cluster3(tmax, tmin, rh)
  else stop("Invalid cluster number")
}
predict_pdi <- function(cluster, tmax, tmin, rh) {
  if (cluster == 1) predict_pdi_cluster1(tmax, tmin, rh)
  else if (cluster == 2) predict_pdi_cluster2(tmax, tmin, rh)
  else if (cluster == 3) predict_pdi_cluster3(tmax, tmin, rh)
  else stop("Invalid cluster number")
}
