# examine_data.R
library(readr)
library(dplyr)

cat("=== EXAMINING YOUR DATASET ===\n")

data_path <- "data/raw/creditcard.csv"
if (file.exists(data_path)) {
  cat("Data file exists:", data_path, "\n")
  
  # Read first few rows to see the structure
  data <- read_csv(data_path, n_max = 10)
  cat("\nDataset structure:\n")
  print(str(data))
  
  cat("\nColumn names:\n")
  print(names(data))
  
  cat("\nFirst few rows:\n")
  print(data)
  
  cat("\nDataset dimensions:\n")
  full_data <- read_csv(data_path)
  cat("Rows:", nrow(full_data), "Columns:", ncol(full_data), "\n")
  
  cat("\nColumn types:\n")
  print(spec(full_data))
  
} else {
  cat("Data file not found:", data_path, "\n")
  cat("Please make sure you have downloaded the correct dataset.\n")
}
