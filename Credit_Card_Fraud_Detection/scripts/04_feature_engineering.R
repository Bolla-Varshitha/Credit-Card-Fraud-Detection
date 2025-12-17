#' Feature Engineering Script - Fixed Version

library(recipes)
library(caret)
library(themis)
library(dplyr)
library(readr)

source("config/config.R")

handle_class_imbalance <- function(train_data, target_column = "Class", method = "downsample") {
  cat("Handling class imbalance using", method, "...\n")
  
  # Ensure target column is factor
  train_data[[target_column]] <- as.factor(train_data[[target_column]])
  
  before_count <- table(train_data[[target_column]])
  cat("Class distribution before balancing:\n")
  print(before_count)
  
  if (method == "SMOTE") {
    # Using themis package for SMOTE
    cat("Using themis package for SMOTE...\n")
    
    recipe_balanced <- recipe(as.formula(paste(target_column, "~ .")), data = train_data) %>%
      step_smote(all_of(target_column), over_ratio = 0.5)
    
    prep_recipe <- prep(recipe_balanced, training = train_data)
    balanced_data <- bake(prep_recipe, new_data = NULL)
    
  } else if (method == "downsample") {
    cat("Using downsampling...\n")
    
    # Ensure we have the target column as factor
    if (!is.factor(train_data[[target_column]])) {
      train_data[[target_column]] <- as.factor(train_data[[target_column]])
    }
    
    # Use caret's downSample function
    balanced_data <- downSample(
      x = train_data %>% select(-all_of(target_column)), 
      y = train_data[[target_column]]
    )
    
    # Rename the Class column that downSample creates
    names(balanced_data)[names(balanced_data) == "Class"] <- target_column
    
  } else if (method == "upsample") {
    cat("Using upsampling...\n")
    
    # Ensure we have the target column as factor
    if (!is.factor(train_data[[target_column]])) {
      train_data[[target_column]] <- as.factor(train_data[[target_column]])
    }
    
    balanced_data <- upSample(
      x = train_data %>% select(-all_of(target_column)), 
      y = train_data[[target_column]]
    )
    
    # Rename the Class column that upSample creates
    names(balanced_data)[names(balanced_data) == "Class"] <- target_column
    
  } else {
    cat("No balancing applied. Using original data.\n")
    balanced_data <- train_data
  }
  
  after_count <- table(balanced_data[[target_column]])
  cat("Class distribution after balancing:\n")
  print(after_count)
  
  return(balanced_data)
}

create_features <- function(data, target_column = "Class") {
  cat("Creating additional features...\n")
  
  # Make a copy to avoid modifying original data
  engineered_data <- data
  
  # Only create Amount_bin if Amount column exists
  if ("Amount" %in% names(engineered_data)) {
    # Create bins for Amount
    engineered_data <- engineered_data %>%
      mutate(
        Amount_bin = cut(Amount, 
                        breaks = quantile(Amount, probs = seq(0, 1, 0.2), na.rm = TRUE),
                        labels = c("Very Low", "Low", "Medium", "High", "Very High"),
                        include.lowest = TRUE),
        
        # Create flag for high amount transactions (top 5%)
        High_Amount = ifelse(Amount > quantile(Amount, 0.95, na.rm = TRUE), 1, 0),
        
        # Log transform amount
        Amount_Log = log1p(Amount + 1)  # +1 to avoid log(0)
      )
  }
  
  # Create PCA magnitude from V features
  v_columns <- grep("^V[0-9]+$", names(engineered_data), value = TRUE)
  if (length(v_columns) >= 3) {
    engineered_data$PCA_magnitude <- sqrt(rowSums(engineered_data[v_columns]^2))
    cat("✓ Created PCA_magnitude feature from", length(v_columns), "V columns\n")
  }
  
  # Create some statistical features
  if (length(v_columns) > 0) {
    engineered_data$V_mean <- rowMeans(engineered_data[v_columns], na.rm = TRUE)
    engineered_data$V_std <- apply(engineered_data[v_columns], 1, sd, na.rm = TRUE)
    cat("✓ Created V_mean and V_std features\n")
  }
  
  # Create interaction features for some important V columns
  if (all(c("V1", "V2") %in% names(engineered_data))) {
    engineered_data$V1_V2_interaction <- engineered_data$V1 * engineered_data$V2
  }
  
  if (all(c("V3", "V4") %in% names(engineered_data))) {
    engineered_data$V3_V4_interaction <- engineered_data$V3 * engineered_data$V4
  }
  
  new_features_count <- ncol(engineered_data) - ncol(data)
  cat("Feature engineering completed. Created", new_features_count, "new features.\n")
  
  return(engineered_data)
}

save_engineered_data <- function(train_balanced, test_data, target_column = "Class") {
  if (!dir.exists(PROCESSED_DATA_DIR)) {
    dir.create(PROCESSED_DATA_DIR, recursive = TRUE)
  }
  
  # Ensure both are data frames
  if (!is.data.frame(train_balanced)) {
    stop("train_balanced is not a data frame")
  }
  if (!is.data.frame(test_data)) {
    stop("test_data is not a data frame")
  }
  
  # Save the data
  write_csv(train_balanced, file.path(PROCESSED_DATA_DIR, "04_train_balanced.csv"))
  write_csv(test_data, file.path(PROCESSED_DATA_DIR, "04_test_processed.csv"))
  
  # Save target column info
  target_info <- data.frame(target_column = target_column)
  write_csv(target_info, file.path(PROCESSED_DATA_DIR, "04_target_info.csv"))
  
  cat("✓ Engineered data saved to processed directory\n")
  cat("  - 04_train_balanced.csv\n")
  cat("  - 04_test_processed.csv\n")
  cat("  - 04_target_info.csv\n")
}

# MAIN EXECUTION
if (sys.nframe() == 0) {
  cat("=== RUNNING FEATURE ENGINEERING SCRIPT DIRECTLY ===\n")
  
  # Load preprocessed data
  train_data_path <- file.path(PROCESSED_DATA_DIR, "02_train_data.csv")
  test_data_path <- file.path(PROCESSED_DATA_DIR, "02_test_data.csv")
  target_info_path <- file.path(PROCESSED_DATA_DIR, "02_target_info.csv")
  
  if (file.exists(train_data_path) && file.exists(test_data_path) && file.exists(target_info_path)) {
    train_data <- read_csv(train_data_path, show_col_types = FALSE)
    test_data <- read_csv(test_data_path, show_col_types = FALSE)
    target_info <- read_csv(target_info_path, show_col_types = FALSE)
    target_column <- target_info$target_column[1]
    
    cat("✓ Loaded preprocessed data\n")
    cat("Training data dimensions:", dim(train_data), "\n")
    cat("Test data dimensions:", dim(test_data), "\n")
    cat("Target column:", target_column, "\n")
    
    # Ensure target column is factor in both datasets
    train_data[[target_column]] <- as.factor(train_data[[target_column]])
    test_data[[target_column]] <- as.factor(test_data[[target_column]])
    
    # Handle class imbalance on training data only
    cat("\nBalancing training data...\n")
    train_balanced <- handle_class_imbalance(train_data, target_column, method = "downsample")
    
    # Check if balancing worked
    if (nrow(train_balanced) == 0) {
      cat("⚠ Balancing failed. Using original training data.\n")
      train_balanced <- train_data
    }
    
    # Apply feature engineering to both train and test
    cat("\nEngineering features for training data...\n")
    train_engineered <- create_features(train_balanced, target_column)
    
    cat("\nEngineering features for test data...\n")
    test_engineered <- create_features(test_data, target_column)
    
    # Ensure target column is preserved as factor
    train_engineered[[target_column]] <- as.factor(train_engineered[[target_column]])
    test_engineered[[target_column]] <- as.factor(test_engineered[[target_column]])
    
    # Save the engineered data
    save_engineered_data(train_engineered, test_engineered, target_column)
    
    cat("\n=== FEATURE ENGINEERING COMPLETED ===\n")
    cat("Training data dimensions:", dim(train_engineered), "\n")
    cat("Test data dimensions:", dim(test_engineered), "\n")
    cat("Training set class distribution:\n")
    print(table(train_engineered[[target_column]]))
    cat("New features:", setdiff(names(train_engineered), names(train_data)), "\n")
    
  } else {
    cat("⚠ Preprocessed data not found. Please run preprocessing script first.\n")
    cat("Missing files:\n")
    if (!file.exists(train_data_path)) cat("  -", train_data_path, "\n")
    if (!file.exists(test_data_path)) cat("  -", test_data_path, "\n")
    if (!file.exists(target_info_path)) cat("  -", target_info_path, "\n")
  }
}
