# Configuration file for Credit Card Fraud Detection Project

# Project paths
PROJECT_ROOT <- getwd()
DATA_DIR <- file.path(PROJECT_ROOT, "data")
RAW_DATA_DIR <- file.path(DATA_DIR, "raw")
PROCESSED_DATA_DIR <- file.path(DATA_DIR, "processed")
SCRIPTS_DIR <- file.path(PROJECT_ROOT, "scripts")
MODELS_DIR <- file.path(PROJECT_ROOT, "models")
REPORTS_DIR <- file.path(PROJECT_ROOT, "reports")

# Data file
DATA_FILE <- "creditcard.csv"

# Random seed for reproducibility
SET_SEED <- 123

# Training parameters
TRAIN_SIZE <- 0.7
CV_FOLDS <- 5

# Model parameters
LOGISTIC_PARAMS <- list(family = "binomial")
RF_PARAMS <- list(ntree = 100, mtry = 5)
XGB_PARAMS <- list(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  objective = "binary:logistic"
)

# Performance metrics
METRICS <- c("Accuracy", "Precision", "Recall", "F1", "AUC")
