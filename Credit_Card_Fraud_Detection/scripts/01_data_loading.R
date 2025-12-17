#' Data Loading Script - Robust Version

library(dplyr)
library(readr)
library(caret)

# Source configuration
source("config/config.R")

load_credit_card_data <- function() {
  data_path <- file.path(RAW_DATA_DIR, DATA_FILE)
  
  if (!file.exists(data_path)) {
    stop("Data file not found: ", data_path)
  }
  
  cat("Loading data from:", data_path, "\n")
  
  # Read data with flexible column types
  credit_data <- read_csv(data_path, show_col_types = FALSE)
  
  cat("✓ Dataset loaded successfully\n")
  cat("Dimensions:", dim(credit_data), "\n")
  cat("Column names:", paste(names(credit_data), collapse = ", "), "\n")
  
  # Check if we have the expected Class column
  if ("Class" %in% names(credit_data)) {
    cat("Class distribution:\n")
    print(table(credit_data$Class))
  } else {
    cat("⚠ WARNING: 'Class' column not found. Available columns:", names(credit_data), "\n")
    cat("This dataset might not be the expected credit card fraud dataset.\n")
  }
  
  return(credit_data)
}

save_loaded_data <- function(data) {
  if (!dir.exists(PROCESSED_DATA_DIR)) {
    dir.create(PROCESSED_DATA_DIR, recursive = TRUE)
  }
  
  output_path <- file.path(PROCESSED_DATA_DIR, "01_loaded_data.csv")
  write_csv(data, output_path)
  cat("✓ Data saved to:", output_path, "\n")
}

# MAIN EXECUTION
if (sys.nframe() == 0) {
  cat("=== RUNNING DATA LOADING SCRIPT DIRECTLY ===\n")
  credit_data <- load_credit_card_data()
  save_loaded_data(credit_data)
  cat("✓ Data loading completed!\n")
}
