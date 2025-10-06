# Advanced analysis using rbsdPredict

# Load required packages
library(rbsdPredict)
library(ggplot2)
library(dplyr)

# Load data
data(rbsd_data)
data(climate_scenarios)

# Preprocess data with different parameters
processed_default <- preprocess_data(rbsd_data)
processed_no_norm <- preprocess_data(rbsd_data, normalize = FALSE)

# Compare clustering results with different methods
cluster_ward <- cluster_analysis(rbsd_data[, c("tmax", "tmin", "RH")], method = "ward.D2")
cluster_complete <- cluster_analysis(rbsd_data[, c("tmax", "tmin", "RH")], method = "complete")

# Train models with different configurations
ml_all <- train_ml_models(processed_default)
ml_subset <- train_ml_models(processed_default, models = c("lm", "rf", "xgbTree"))

# Compare model performance
eval_all <- evaluate_model(ml_all)
eval_subset <- evaluate_model(ml_subset)

# Train hybrid models with different cluster numbers
hybrid_3 <- train_hybrid_model(rbsd_data, n_clusters = 3)
hybrid_4 <- train_hybrid_model(rbsd_data, n_clusters = 4)

# Compare hybrid model performance
cat("3-cluster hybrid model performance:\n")
print(hybrid_3$metrics)

cat("\n4-cluster hybrid model performance:\n")
print(hybrid_4$metrics)

# Create comprehensive risk assessment
risk_map_A <- plot_risk_map(rbsd_data, hybrid_3, "A")
risk_map_PDI <- plot_risk_map(rbsd_data, hybrid_3, "PDI")

# Save plots
ggsave("risk_map_A.png", risk_map_A, width = 8, height = 6, dpi = 300)
ggsave("risk_map_PDI.png", risk_map_PDI, width = 8, height = 6, dpi = 300)

# Create scenario analysis
scenario_predictions <- data.frame()

for (scenario in rownames(climate_scenarios)) {
  scenario_data <- climate_scenarios[scenario, ]
  
  # Predict with ML models
  pred_A_ml <- predict_rbsd(ml_all, scenario_data, "A")
  pred_PDI_ml <- predict_rbsd(ml_all, scenario_data, "PDI")
  
  # Predict with hybrid model
  pred_A_hybrid <- predict_rbsd(hybrid_3, scenario_data, "A")
  pred_PDI_hybrid <- predict_rbsd(hybrid_3, scenario_data, "PDI")
  
  # Combine results
  scenario_predictions <- rbind(scenario_predictions, data.frame(
    Scenario = scenario,
    A_ML = pred_A_ml,
    PDI_ML = pred_PDI_ml,
    A_Hybrid = pred_A_hybrid,
    PDI_Hybrid = pred_PDI_hybrid
  ))
}

# Print scenario analysis results
print(scenario_predictions)

# Create visualization of scenario results
scenario_melted <- reshape2::melt(scenario_predictions, id.vars = "Scenario")

p <- ggplot(scenario_melted, aes(x = Scenario, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Scenario Analysis: ML vs Hybrid Models",
       x = "Climate Scenario", y = "Predicted Value") +
  theme_minimal()

print(p)

# Save scenario analysis plot
ggsave("scenario_analysis.png", p, width = 10, height = 6, dpi = 300)
