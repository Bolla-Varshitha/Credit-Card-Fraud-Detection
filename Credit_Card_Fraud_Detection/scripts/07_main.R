#' Main Script - Orchestrates the entire project workflow

cat("Starting Credit Card Fraud Detection Project...\n")

# Set working directory to project root
setwd("C:/Users/admin/OneDrive/Desktop/Credit Card Fraud Detection")
cat("Working directory set to:", getwd(), "\n")

# Source configuration
source("config/config.R")

# Create directories if they don't exist
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(RAW_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PROCESSED_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(MODELS_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(REPORTS_DIR, showWarnings = FALSE, recursive = TRUE)

# Execute pipeline with explicit saving
cat("\n=== STEP 1: Data Loading ===\n")
source("scripts/01_data_loading.R")
credit_data <- load_credit_card_data()
save_loaded_data(credit_data)

cat("\n=== STEP 2: Data Preprocessing ===\n")
source("scripts/02_data_preprocessing.R")

# Load the data from step 1
loaded_data <- read_csv(file.path(PROCESSED_DATA_DIR, "01_loaded_data.csv"), show_col_types = FALSE)

# Preprocess data
preprocessing_result <- preprocess_data(loaded_data)
preprocessed_data <- preprocessing_result$data
target_column <- preprocessing_result$target

# Split data
split_data_list <- split_data(preprocessed_data, target_column)
save_preprocessed_data(split_data_list)

cat("\n=== STEP 3: Exploratory Data Analysis ===\n")
source("scripts/03_exploratory_analysis.R")

# Load training data for EDA
train_data <- read_csv(file.path(PROCESSED_DATA_DIR, "02_train_data.csv"), show_col_types = FALSE)
target_info <- read_csv(file.path(PROCESSED_DATA_DIR, "02_target_info.csv"), show_col_types = FALSE)
target_column_eda <- target_info$target_column[1]

perform_eda(train_data, target_column_eda)

cat("\n=== STEP 4: Feature Engineering ===\n")
source("scripts/04_feature_engineering.R")

# Load the data needed for feature engineering
train_data <- read_csv(file.path(PROCESSED_DATA_DIR, "02_train_data.csv"), show_col_types = FALSE)
test_data <- read_csv(file.path(PROCESSED_DATA_DIR, "02_test_data.csv"), show_col_types = FALSE)
target_info <- read_csv(file.path(PROCESSED_DATA_DIR, "02_target_info.csv"), show_col_types = FALSE)
target_column_fe <- target_info$target_column[1]

# Handle class imbalance and feature engineering
train_balanced <- handle_class_imbalance(train_data, target_column_fe, method = "downsample")
train_engineered <- create_features(train_balanced, target_column_fe)
test_engineered <- create_features(test_data, target_column_fe)
save_engineered_data(train_engineered, test_engineered, target_column_fe)

cat("\n=== STEP 5: Model Training ===\n")
source("scripts/05_model_training.R")

# Load engineered data
train_engineered <- read_csv(file.path(PROCESSED_DATA_DIR, "04_train_balanced.csv"), show_col_types = FALSE)
test_engineered <- read_csv(file.path(PROCESSED_DATA_DIR, "04_test_processed.csv"), show_col_types = FALSE)
target_info <- read_csv(file.path(PROCESSED_DATA_DIR, "04_target_info.csv"), show_col_types = FALSE)
target_column_mt <- target_info$target_column[1]

# Convert character columns to factors
train_engineered <- train_engineered %>% mutate_if(is.character, as.factor)
test_engineered <- test_engineered %>% mutate_if(is.character, as.factor)

# Ensure target is factor
train_engineered[[target_column_mt]] <- as.factor(train_engineered[[target_column_mt]])
test_engineered[[target_column_mt]] <- as.factor(test_engineered[[target_column_mt]])

# Train models
training_results <- train_models(train_engineered, test_engineered, target_column_mt)
save_models(training_results$models)
save_predictions(training_results$predictions)

cat("\n=== STEP 6: Model Evaluation ===\n")
source("scripts/06_model_evaluation.R")

# Load models and predictions
models <- list()
model_files <- list.files(MODELS_DIR, pattern = "05_.*_model\\.rds", full.names = TRUE)

for (file in model_files) {
  model_name <- gsub("05_(.*)_model\\.rds", "\\1", basename(file))
  models[[model_name]] <- readRDS(file)
}

predictions <- readRDS(file.path(PROCESSED_DATA_DIR, "05_predictions.rds"))
test_data <- read_csv(file.path(PROCESSED_DATA_DIR, "04_test_processed.csv"), show_col_types = FALSE)
target_info <- read_csv(file.path(PROCESSED_DATA_DIR, "04_target_info.csv"), show_col_types = FALSE)
target_column_me <- target_info$target_column[1]

# Convert test data target to factor
test_data[[target_column_me]] <- as.factor(test_data[[target_column_me]])

# Evaluate models
evaluation_results <- evaluate_models(models, predictions, test_data, target_column_me)
plots <- create_evaluation_plots(evaluation_results)
save_evaluation_results(evaluation_results)

cat("\n=== PROJECT COMPLETED SUCCESSFULLY ===\n")
cat("Outputs available in:\n")
cat("- Processed data: data/processed/\n")
cat("- Trained models: models/\n")
cat("- Reports and plots: reports/\n")

# List the generated files
cat("\n=== GENERATED FILES ===\n")
cat("Processed data files:\n")
print(list.files(PROCESSED_DATA_DIR))

cat("\nModel files:\n")
print(list.files(MODELS_DIR))

cat("\nReport files:\n")
print(list.files(REPORTS_DIR))

cat("\n🎉 PROJECT EXECUTION COMPLETED! 🎉\n")
