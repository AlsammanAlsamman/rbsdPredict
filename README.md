# rbsdPredict: AI-Driven Rice Brown Spot Disease Forecasting

## Overview
`rbsdPredict` is an open-source R package developed as part of the research article:

**Integrating Artificial Intelligence and Climate Data for Predicting Rice Brown Spot Disease Dynamics**  
Sudhasha Selvaraj, Alsamman M. Alsamman, Tuyen V. Ha, Shalini Gakhar, Suresh Kumar S. Monikandan, Ajit Govind*  
Rice Research Station, Tamil Nadu Agricultural University, India  
International Center for Agricultural Research in the Dry Areas, Lebanon  
International Rice Research Institute, Philippines  
Faculty of Resources Management, Thai Nguyen University of Agriculture and Forestry, Vietnam  
Correspondence: Ajit Govind, a.govind@cgiar.org

## Abstract
Rice Brown Spot Disease (RBSD), caused by Bipolaris oryzae, has historically triggered major famines in India. This package implements predictive models for RBSD using AI/ML approaches, leveraging multi-year field and climate data. It enables robust forecasting of both aerospora concentration and disease intensity (PDI) using meteorological variables, and applies these models to gridded climate data under IPCC scenarios to map spatio-temporal risk across Eastern India. The package empowers stakeholders with actionable intelligence for early warning and hotspot identification, supporting food security under climate change.

## Features
- Data preprocessing and cleaning utilities
- Hierarchical clustering of environmental data
- Machine learning models (linear regression, random forest, neural networks, etc.)
- Hybrid modeling combining clustering and regression
- Prediction functions for aerospora concentration and disease intensity
- Visualization tools for risk maps, boxplots, and actual vs predicted plots
- Ready-to-use datasets (`rbsd_data`)

## Installation
```r
# Install devtools if needed
install.packages("devtools")
devtools::install_local("/path/to/rbsdPredict")
```

## Usage Example
```r
library(rbsdPredict)

# Load the main dataset
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
```

## Data Sources
- **Field Data**: Multi-year epidemiological studies in Southern India
- **Climate Scenarios**: IPCC projections and regional meteorological data

## Citation
If you use this package, please cite the article above.

## License
See `LICENSE` file for details.
