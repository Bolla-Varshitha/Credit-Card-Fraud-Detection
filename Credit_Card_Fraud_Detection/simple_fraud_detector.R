# simple_fraud_detector.R
#' Simple Credit Card Fraud Detector
#' For end users who want to check transactions

# Load required libraries and model
library(caret)
library(dplyr)

cat("🔍 Credit Card Fraud Detection System\n")
cat("=====================================\n\n")

# Load the best model (XGBoost from your results)
model <- readRDS("models/05_xgb_model.rds")

# Function to check a single transaction
check_transaction <- function() {
  cat("Enter transaction details:\n")
  
  # In a real application, you'd get this from user input or a file
  # For demonstration, using example data
  transaction_data <- data.frame(
    V1 = -1.359807, V2 = -0.072781, V3 = 2.536347, V4 = 1.378155,
    V5 = -0.338321, V6 = 0.462388, V7 = 0.239599, V8 = 0.098698,
    V9 = 0.363787, V10 = 0.090794, V11 = -0.551600, V12 = -0.617801,
    V13 = -0.991390, V14 = -0.311169, V15 = 1.468177, V16 = -0.470401,
    V17 = 0.207971, V18 = 0.025791, V19 = 0.403993, V20 = 0.251412,
    V21 = -0.018307, V22 = 0.277838, V23 = -0.110474, V24 = 0.066928,
    V25 = 0.128539, V26 = -0.189115, V27 = 0.133558, V28 = -0.021053,
    Amount = 149.62,
    High_Amount = 0, 
    Amount_Log = log(149.62 + 1),
    PCA_magnitude = 12.345, 
    V_mean = 0.123, 
    V_std = 0.456
  )
  
  # Make prediction
  prediction <- predict(model, transaction_data)
  probability <- predict(model, transaction_data, type = "prob")$Fraud
  
  # Display results
  cat("\n" + strrep("=", 50) + "\n")
  cat("FRAUD DETECTION RESULTS\n")
  cat(strrep("=", 50) + "\n")
  cat("Transaction Amount: $", transaction_data$Amount, "\n")
  cat("Prediction:", as.character(prediction), "\n")
  cat("Fraud Probability:", round(probability * 100, 2), "%\n")
  
  if (probability > 0.5) {
    cat("🚨 ALERT: HIGH RISK OF FRAUD!\n")
    cat("Recommended action: Block transaction and contact cardholder\n")
  } else if (probability > 0.3) {
    cat("⚠️  WARNING: Moderate risk\n")
    cat("Recommended action: Additional verification required\n")
  } else {
    cat("✅ LOW RISK: Transaction appears legitimate\n")
    cat("Recommended action: Process normally\n")
  }
  cat(strrep("=", 50) + "\n")
}

# Function to process a CSV file
process_file <- function(file_path) {
  cat("Processing file:", file_path, "\n")
  
  # Read transactions
  transactions <- read.csv(file_path)
  
  # Add engineered features
  source("scripts/04_feature_engineering.R")
  transactions_processed <- create_features(transactions)
  
  # Make predictions
  predictions <- predict(model, transactions_processed)
  probabilities <- predict(model, transactions_processed, type = "prob")$Fraud
  
  # Create results
  results <- data.frame(
    TransactionID = 1:nrow(transactions),
    Amount = transactions$Amount,
    FraudProbability = round(probabilities * 100, 2),
    Status = ifelse(probabilities > 0.5, "FRAUD", "LEGITIMATE"),
    RiskLevel = cut(probabilities, 
                   breaks = c(0, 0.1, 0.3, 0.5, 1),
                   labels = c("Low", "Medium", "High", "Very High"))
  )
  
  # Save results
  output_file <- "fraud_analysis_results.csv"
  write.csv(results, output_file, row.names = FALSE)
  
  # Summary
  cat("\n" + strrep("=", 50) + "\n")
  cat("BATCH ANALYSIS COMPLETE\n")
  cat(strrep("=", 50) + "\n")
  cat("Total transactions processed:", nrow(results), "\n")
  cat("Fraud alerts:", sum(results$Status == "FRAUD"), "\n")
  cat("Total amount at risk: $", 
      sum(results$Amount[results$Status == "FRAUD"]), "\n")
  cat("Results saved to:", output_file, "\n")
  
  return(results)
}

# Main menu
cat("How would you like to use the fraud detection system?\n")
cat("1. Check a single transaction\n")
cat("2. Process a CSV file of transactions\n")
cat("3. View system performance\n")

# In a real app, you'd get user input here
choice <- 1  # Example choice

if (choice == 1) {
  check_transaction()
} else if (choice == 2) {
  # Example file processing
  # process_file("new_transactions.csv")
  cat("File processing feature ready - specify your CSV file path\n")
} else if (choice == 3) {
  # Show model performance
  cat("\nSystem Performance:\n")
  cat("Model: XGBoost (Best performing model)\n")
  cat("AUC: 98.01%\n")
  cat("Accuracy: 96.37%\n")
  cat("Recall: 89.80%\n")
  cat("This system can detect 89.8% of fraudulent transactions\n")
}
