# Basic usage of the rbsdPredict package

# Load the package
library(rbsdPredict)

# Load example data
data(rbsd_data)

# View the structure of the data
str(rbsd_data)

# Preprocess the data
processed <- preprocess_data(rbsd_data)

# Perform clustering analysis
cluster_result <- cluster_analysis(rbsd_data[, c("tmax", "tmin", "RH")])

# Plot dendrogram
plot_dendrogram(cluster_result)

# Plot cluster boxplots
p <- plot_cluster_boxplots(cluster_result, rbsd_data[, c("tmax", "tmin", "RH")])
print(p)

# Train machine learning models
ml_models <- train_ml_models(processed)

# Evaluate models
evaluation <- evaluate_model(ml_models)
print(evaluation)

# Train hybrid model
hybrid_model <- train_hybrid_model(rbsd_data)

# Make predictions
predictions_ml <- predict_rbsd(ml_models, processed$test_features, "A")
predictions_hybrid <- predict_rbsd(hybrid_model, processed$test_features, "A")

# Plot actual vs predicted
p1 <- plot_actual_vs_predicted(processed$test_targets$A, predictions_ml, "A (ML)")
print(p1)

p2 <- plot_actual_vs_predicted(processed$test_targets$A, predictions_hybrid, "A (Hybrid)")
print(p2)

# Create risk map
risk_map <- plot_risk_map(rbsd_data[, c("tmax", "tmin", "RH")], ml_models, "A")
print(risk_map)
