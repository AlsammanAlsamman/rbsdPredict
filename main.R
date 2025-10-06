# Load the package
library(rbsdPredict)

# Load the main dataset (replace with your actual data)
data(rbsd_data)

# Preprocess the data
processed <- preprocess_data(rbsd_data)

# Train machine learning models
ml_models <- train_ml_models(processed, models = c("lm", "rf", "xgbTree", "knn", "nnet"))

# Predict RBSD for test features (type can be "A" or "PDI")
predictions_A <- predict_rbsd(ml_models, processed$test_features, "A")
predictions_PDI <- predict_rbsd(ml_models, processed$test_features, "PDI")

# Print predictions
print(predictions_A)
print(predictions_PDI)
