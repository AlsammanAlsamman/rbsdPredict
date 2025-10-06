# rbsdPredict: Weather-Driven Epidemiology of Rice Brown Spot Disease

## Overview
`rbsdPredict` is an R package for modeling, predicting, and visualizing the spatio-temporal risk of Rice Brown Spot Disease (RBSD) in India. It leverages AI/ML approaches and hierarchical clustering to analyze field and climate scenario data, supporting early warning and risk mapping for rice disease outbreaks.

## Features
- Data processing and cleaning utilities
- Clustering and classification of environmental data
- Machine learning models (linear regression, random forest, SVM)
- Hybrid modeling with cluster-specific regression equations
- Prediction functions for disease intensity and aerospora concentration
- Visualization tools for boxplots, scatterplots, and risk maps
- Ready-to-use datasets (`rbsd_data`, `climate_scenarios`)

## Installation
Clone the repository and use `devtools` to install:
```r
# Install devtools if needed
install.packages("devtools")
devtools::install_local("/path/to/rbsdPredict")
```

## Usage
```r
library(rbsdPredict)
# Load the main dataset
data(rbsd_data)
head(rbsd_data)

# Run clustering and prediction
clustered <- data_predict_clusters(rbsd_data)
predicted <- predict_a_pdi(clustered)

# Visualize results
plot_boxplot_clusters(clustered)
plots <- plot_actual_vs_predicted(predicted)
print(plots$A_plot)
print(plots$PDI_plot)

# Plot risk map (example)
# plot_risk_map(climate_scenarios, trained_model)
```

## Data Sources
- **Field Data**: Collected from multi-year epidemiological studies in Southern India.
- **Climate Scenarios**: Derived from IPCC projections and regional meteorological data.
- **Reference**: Modelling and Scaling Weather-Driven Epidemiology of Rice Brown Spot Disease in Peninsular India (Sudhasha Selvaraj et al.)

## Authors
- Sudhasha Selvaraj
- Alsamman M. Alsamman
- Tuyen V. Ha
- Shalini Gakhar
- Suresh Kumar S. Monikandan
- Ajit Govind (Corresponding: a.govind@cgiar.org)

## License
See `LICENSE` file for details.

## Citation
If you use this package, please cite:
> Sudhasha Selvaraj, Alsamman M. Alsamman, Tuyen V. Ha, Shalini Gakhar, Suresh Kumar S. Monikandan, Ajit Govind. Modelling and Scaling Weather-Driven Epidemiology of Rice Brown Spot Disease in Peninsular India.
