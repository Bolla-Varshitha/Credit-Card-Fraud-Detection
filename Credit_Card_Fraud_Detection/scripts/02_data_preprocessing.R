#' Data Preprocessing Script - Robust Version

library(dplyr)
library(caret)
library(recipes)
library(readr)

source("config/config.R")

preprocess_data <- function(data) {
  cat("Starting data preprocessing...\n")
  cat("Input data dimensions:", dim(data), "\n")
  cat("Input data columns:", names(data), "\n")
  
  # Remove unnecessary columns if they exist
  columns_to_remove <- c("Time")
  existing_columns <- columns_to_remove[columns_to_remove %in% names(data)]
  if (length(existing_columns) > 0) {
    data <- data %>% select(-all_of(existing_columns))
    cat("✓ Removed columns:", paste(existing_columns, collapse = ", "), "\n")
  }
  
  # Check if we have a target column
  target_column <- NULL
  if ("Class" %in% names(data)) {
    target_column <- "Class"
    # Convert Class to factor
    data$Class <- as.factor(data$Class)
    levels(data$Class) <- c("Legitimate", "Fraud")
    cat("✓ Converted Class to factor\n")
  } else {
    stop("Class column not found in dataset")
  }
  
  # Scale numeric columns (excluding target)
  numeric_columns <- names(data)[sapply(data, is.numeric)]
  if (length(numeric_columns) > 0) {
    # Remove target from numeric columns if it's numeric
    numeric_columns <- setdiff(numeric_columns, target_column)
    
    if (length(numeric_columns) > 0) {
      preproc_recipe <- recipe(as.formula(paste(target_column, "~ .")), data = data) %>%
        step_scale(all_of(numeric_columns)) %>%
        step_center(all_of(numeric_columns))
      
      preprocessed_data <- prep(preproc_recipe) %>% bake(new_data = data)
      cat("✓ Scaled numeric columns:", paste(numeric_columns, collapse = ", "), "\n")
    } else {
      preprocessed_data <- data
    }
  } else {
    preprocessed_data <- data
  }
  
  cat("✓ Data preprocessing completed\n")
  
  # Return both the data and target column name as a list
  return(list(data = preprocessed_data, target = target_column))
}

split_data <- function(data, target_column) {
  set.seed(SET_SEED)
  
  # Check if we have enough data for splitting
  if (nrow(data) < 2) {
    stop("Not enough data for splitting. Only", nrow(data), "rows available.")
  }
  
  # Check if target column has at least 2 classes
  if (length(unique(data[[target_column]])) < 2) {
    stop("Target column", target_column, "has only one class. Need at least 2 classes for classification.")
  }
  
  train_index <- createDataPartition(
    data[[target_column]], 
    p = TRAIN_SIZE, 
    list = FALSE
  )
  
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  cat("✓ Data split completed\n")
  cat("Training set:", nrow(train_data), "rows\n")
  cat("Test set:", nrow(test_data), "rows\n")
  cat("Training class distribution:\n")
  print(table(train_data[[target_column]]))
  
  return(list(train = train_data, test = test_data, target = target_column))
}

save_preprocessed_data <- function(data_list) {
  if (!dir.exists(PROCESSED_DATA_DIR)) {
    dir.create(PROCESSED_DATA_DIR, recursive = TRUE)
  }
  
  write_csv(data_list$train, file.path(PROCESSED_DATA_DIR, "02_train_data.csv"))
  write_csv(data_list$test, file.path(PROCESSED_DATA_DIR, "02_test_data.csv"))
  
  # Save target column info
  target_info <- data.frame(target_column = data_list$target)
  write_csv(target_info, file.path(PROCESSED_DATA_DIR, "02_target_info.csv"))
  
  cat("✓ Preprocessed data saved to processed directory\n")
}

# MAIN EXECUTION
if (sys.nframe() == 0) {
  cat("=== RUNNING DATA PREPROCESSING SCRIPT DIRECTLY ===\n")
  
  # Load the data from previous step
  loaded_data_path <- file.path(PROCESSED_DATA_DIR, "01_loaded_data.csv")
  if (file.exists(loaded_data_path)) {
    raw_data <- read_csv(loaded_data_path, show_col_types = FALSE)
    cat("✓ Loaded data from:", loaded_data_path, "\n")
  } else {
    # If no saved data, load from raw
    source("scripts/01_data_loading.R")
    raw_data <- load_credit_card_data()
  }
  
  preprocessing_result <- preprocess_data(raw_data)
  split_data_list <- split_data(preprocessing_result$data, preprocessing_result$target)
  save_preprocessed_data(split_data_list)
  cat("✓ Data preprocessing completed!\n")
}
