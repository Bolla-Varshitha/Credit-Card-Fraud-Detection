#' Model Training Script - Fixed Version

library(caret)
library(randomForest)
library(xgboost)
library(glmnet)
library(pROC)
library(dplyr)
library(readr)

source("config/config.R")

train_models <- function(train_data, test_data, target_column = "Class") {
  cat("Starting model training...\n")
  cat("Target column:", target_column, "\n")
  
  # Define control parameters for training
  train_control <- trainControl(
    method = "cv",
    number = CV_FOLDS,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "up",
    verboseIter = FALSE,
    savePredictions = TRUE
  )
  
  models <- list()
  predictions <- list()
  
  # Prepare formula
  model_formula <- as.formula(paste(target_column, "~ ."))
  
  # 1. Logistic Regression
  cat("1. Training Logistic Regression...\n")
  tryCatch({
    set.seed(SET_SEED)
    models$logistic <- train(
      model_formula,
      data = train_data,
      method = "glm",
      family = "binomial",
      trControl = train_control,
      metric = "ROC"
    )
    predictions$logistic <- predict(models$logistic, newdata = test_data)
    predictions$logistic_probs <- predict(models$logistic, newdata = test_data, type = "prob")$Fraud
    cat("✓ Logistic Regression completed\n")
  }, error = function(e) {
    cat("✗ Logistic Regression failed:", e$message, "\n")
  })
  
  # 2. Random Forest (with smaller dataset for speed)
  cat("2. Training Random Forest...\n")
  tryCatch({
    set.seed(SET_SEED)
    # Use smaller sample for faster training during testing
    if (nrow(train_data) > 10000) {
      train_sample <- train_data[sample(nrow(train_data), 10000), ]
      cat("  Using sample of 10,000 rows for faster training\n")
    } else {
      train_sample <- train_data
    }
    
    models$random_forest <- train(
      model_formula,
      data = train_sample,
      method = "rf",
      trControl = train_control,
      metric = "ROC",
      ntree = 50,  # Reduced for speed
      tuneLength = 2
    )
    predictions$random_forest <- predict(models$random_forest, newdata = test_data)
    predictions$random_forest_probs <- predict(models$random_forest, newdata = test_data, type = "prob")$Fraud
    cat("✓ Random Forest completed\n")
  }, error = function(e) {
    cat("✗ Random Forest failed:", e$message, "\n")
  })
  
  # 3. XGBoost
  cat("3. Training XGBoost...\n")
  tryCatch({
    set.seed(SET_SEED)
    models$xgb <- train(
      model_formula,
      data = train_data,
      method = "xgbTree",
      trControl = train_control,
      metric = "ROC",
      tuneLength = 2,
      verbose = FALSE
    )
    predictions$xgb <- predict(models$xgb, newdata = test_data)
    predictions$xgb_probs <- predict(models$xgb, newdata = test_data, type = "prob")$Fraud
    cat("✓ XGBoost completed\n")
  }, error = function(e) {
    cat("✗ XGBoost failed:", e$message, "\n")
  })
  
  # 4. Support Vector Machine (with smaller sample)
  cat("4. Training Support Vector Machine...\n")
  tryCatch({
    set.seed(SET_SEED)
    if (nrow(train_data) > 5000) {
      train_sample <- train_data[sample(nrow(train_data), 5000), ]
      cat("  Using sample of 5,000 rows for faster training\n")
    } else {
      train_sample <- train_data
    }
    
    models$svm <- train(
      model_formula,
      data = train_sample,
      method = "svmRadial",
      trControl = train_control,
      metric = "ROC",
      tuneLength = 2
    )
    predictions$svm <- predict(models$svm, newdata = test_data)
    predictions$svm_probs <- predict(models$svm, newdata = test_data, type = "prob")$Fraud
    cat("✓ SVM completed\n")
  }, error = function(e) {
    cat("✗ SVM failed:", e$message, "\n")
  })
  
  return(list(models = models, predictions = predictions))
}

save_models <- function(models) {
  if (!dir.exists(MODELS_DIR)) {
    dir.create(MODELS_DIR, recursive = TRUE)
  }
  
  for (model_name in names(models)) {
    if (!is.null(models[[model_name]])) {
      model_path <- file.path(MODELS_DIR, paste0("05_", model_name, "_model.rds"))
      saveRDS(models[[model_name]], model_path)
      cat("✓ Saved", model_name, "model\n")
    }
  }
  cat("✓ All models saved to models directory\n")
}

save_predictions <- function(predictions) {
  if (!dir.exists(PROCESSED_DATA_DIR)) {
    dir.create(PROCESSED_DATA_DIR, recursive = TRUE)
  }
  
  predictions_path <- file.path(PROCESSED_DATA_DIR, "05_predictions.rds")
  saveRDS(predictions, predictions_path)
  cat("✓ Predictions saved to:", predictions_path, "\n")
}

# MAIN EXECUTION
if (sys.nframe() == 0) {
  cat("=== RUNNING MODEL TRAINING SCRIPT DIRECTLY ===\n")
  
  # Load engineered data
  train_data_path <- file.path(PROCESSED_DATA_DIR, "04_train_balanced.csv")
  test_data_path <- file.path(PROCESSED_DATA_DIR, "04_test_processed.csv")
  target_info_path <- file.path(PROCESSED_DATA_DIR, "04_target_info.csv")
  
  if (file.exists(train_data_path) && file.exists(test_data_path) && file.exists(target_info_path)) {
    train_data <- read_csv(train_data_path, show_col_types = FALSE)
    test_data <- read_csv(test_data_path, show_col_types = FALSE)
    target_info <- read_csv(target_info_path, show_col_types = FALSE)
    target_column <- target_info$target_column[1]
    
    cat("✓ Loaded engineered data\n")
    cat("Training data dimensions:", dim(train_data), "\n")
    cat("Test data dimensions:", dim(test_data), "\n")
    cat("Target column:", target_column, "\n")
    
    # Convert character columns to factors if any
    train_data <- train_data %>% mutate_if(is.character, as.factor)
    test_data <- test_data %>% mutate_if(is.character, as.factor)
    
    # Ensure target is factor
    train_data[[target_column]] <- as.factor(train_data[[target_column]])
    test_data[[target_column]] <- as.factor(test_data[[target_column]])
    
    # Train models
    training_results <- train_models(train_data, test_data, target_column)
    
    # Save models and predictions
    save_models(training_results$models)
    save_predictions(training_results$predictions)
    
    cat("\n=== MODEL TRAINING COMPLETED ===\n")
    cat("Trained models:", names(training_results$models), "\n")
    
  } else {
    cat("⚠ Engineered data not found. Please run feature engineering script first.\n")
    cat("Missing files:\n")
    if (!file.exists(train_data_path)) cat("  -", train_data_path, "\n")
    if (!file.exists(test_data_path)) cat("  -", test_data_path, "\n")
    if (!file.exists(target_info_path)) cat("  -", target_info_path, "\n")
  }
}
