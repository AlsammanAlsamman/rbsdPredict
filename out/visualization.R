# Visualization functions for RBSD project
library(ggplot2)
library(reshape2)
library(rstatix)

# Boxplot by cluster
plot_boxplot_clusters <- function(data) {
  data_melted <- melt(data, id.vars = "cluster", measure.vars = c("tmax", "tmin", "RH"))
  ggplot(data_melted, aes(x = .data$variable, y = .data$value, fill = as.factor(.data$cluster))) +
    geom_boxplot() +
    facet_wrap(~.data$cluster) +
    theme_minimal()
}

# Scatter plot actual vs predicted
plot_actual_vs_predicted <- function(data) {
  p1 <- ggplot(data, aes(x = .data$A, y = .data$predicted_A)) +
    geom_point(alpha = 0.6) +
    labs(title = "Actual vs Predicted A", x = "Actual A", y = "Predicted A") +
    theme_minimal()
  p2 <- ggplot(data, aes(x = .data$PDI, y = .data$predicted_PDI)) +
    geom_point(alpha = 0.6) +
    labs(title = "Actual vs Predicted PDI", x = "Actual PDI", y = "Predicted PDI") +
    theme_minimal()
  return(list(A_plot = p1, PDI_plot = p2))
}
