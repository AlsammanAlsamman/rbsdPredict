# Load the package
library(rbsdPredict)

# Load the main dataset (replace with your actual data)
data(rbsd_data)

# Preprocess the data
processed <- preprocess_data(rbsd_data)

# Select only environmental predictors for training and testing
predictor_cols <- c("tmax", "tmin", "RH", "RF", "PET", "WS")
train_features <- processed$train_features[, predictor_cols]
test_features <- processed$test_features[, predictor_cols]

# Train machine learning models (use processed directly)
ml_models <- train_ml_models(processed, models = c("lm", "rf", "xgbTree", "knn", "nnet"))

# Predict RBSD for test features (type can be "A" or "PDI")
predictions_A <- predict_rbsd(ml_models, test_features, "A")
predictions_PDI <- predict_rbsd(ml_models, test_features, "PDI")

# Print predictions
print(predictions_A)
print(predictions_PDI)

# Visualize actual vs predicted for "A"
library(ggplot2)
plot_actual_vs_predicted(processed$test_targets$A, predictions_A, "A")

# Visualize actual vs predicted for "PDI"
plot_actual_vs_predicted(processed$test_targets$PDI, predictions_PDI, "PDI")

