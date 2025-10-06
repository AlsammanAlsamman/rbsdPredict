# Machine learning models for RBSD project
library(caret)
library(randomForest)
library(e1071)

#' Train and evaluate regression models within each cluster
#' @param data Data frame
#' @param target Target variable name
#' @param model_type Model type: "linear", "randomForest", "svm"
#' @return List with RMSE, R-squared, and model object
train_evaluate_model <- function(data, target, model_type = "linear") {
  set.seed(123)
  trainIndex <- caret::createDataPartition(data[[target]], p = 0.8, list = FALSE)
  data_train <- data[trainIndex, ]
  data_test <- data[-trainIndex, ]
  
  formula <- as.formula(paste(target, "~ tmax + tmin + RH"))
  
  # Define model functions and their arguments
  model_specs <- list(
    linear = list(fun = stats::lm, args = list(formula = formula, data = data_train)),
    randomForest = list(fun = randomForest::randomForest, args = list(formula = formula, data = data_train, ntree = 100)),
    svm = list(fun = e1071::svm, args = list(formula = formula, data = data_train))
  )
  
  if (!model_type %in% names(model_specs)) {
    stop("Unsupported model_type. Choose from: ", paste(names(model_specs), collapse = ", "))
  }
  
  # Dynamically call the model function
  spec <- model_specs[[model_type]]
  model <- do.call(spec$fun, spec$args)
  
  predictions <- predict(model, newdata = data_test)
  
  # Calculate metrics
  rmse <- sqrt(mean((data_test[[target]] - predictions)^2))
  r_squared <- cor(data_test[[target]], predictions)^2
  
  return(list(rmse = rmse, r_squared = r_squared, model = model))
}
