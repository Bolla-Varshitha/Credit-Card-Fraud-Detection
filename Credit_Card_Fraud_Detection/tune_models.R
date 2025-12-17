# Fine-tune hyperparameters for better performance
# Create tune_models.R
library(caret)
library(mlr)

# Hyperparameter tuning for XGBoost
tune_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 1),
  colsample_bytree = c(0.6, 0.8, 1.0),
  min_child_weight = c(1, 3),
  subsample = c(0.5, 0.8, 1.0)
)

# Use this for better model performance
