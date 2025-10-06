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

  if (model_type == "linear") {
    model <- stats::lm(
      as.formula(paste(target, "~ tmax + tmin + RH")),
      data = data_train
    )
    predictions <- predict(
      model,
      newdata = data_test
    )
  } else if (model_type == "randomForest") {
    model <- randomForest::randomForest(
      as.formula(paste(target, "~ tmax + tmin + RH")),
      data = data_train,
      ntree = 100
    )
    predictions <- predict(
      model,
      newdata = data_test
    )
  } else if (model_type == "svm") {
    model <- e1071::svm(
      as.formula(paste(target, "~ tmax + tmin + RH")),
      data = data_train
    )
    predictions <- predict(
      model,
      newdata = data_test
    )
  }

  rmse <- sqrt(mean((data_test[[target]] - predictions)^2))
  r_squared <- cor(data_test[[target]], predictions)^2
  return(list(rmse = rmse, r_squared = r_squared, model = model))
}
