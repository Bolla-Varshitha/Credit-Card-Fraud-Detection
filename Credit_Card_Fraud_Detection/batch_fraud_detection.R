# batch_fraud_detection.R - Process multiple transactions (Complete Fix)
library(caret)
library(dplyr)
library(readr)

cat("📊 Batch Fraud Detection System\n")
cat("===============================\n\n")

# Load the trained model
model <- readRDS("models/05_xgb_model.rds")
cat("✓ Loaded trained model\n")

# Complete feature engineering function (matches training exactly)
create_features_for_prediction <- function(data) {
  engineered_data <- data
  
  # Create the same features as in training
  if ("Amount" %in% names(engineered_data)) {
    # Use fixed breaks that work for any dataset size
    amount_breaks <- c(0, 10, 50, 100, 500, 1000, 10000)
    amount_labels <- c("Very Low", "Low", "Medium", "High", "Very High", "Extreme")
    
    engineered_data$Amount_bin <- cut(
      engineered_data$Amount,
      breaks = amount_breaks,
      labels = amount_labels,
      include.lowest = TRUE
    )
    
    # Use fixed threshold for High_Amount
    engineered_data$High_Amount <- as.integer(engineered_data$Amount > 100)
    engineered_data$Amount_Log <- log1p(engineered_data$Amount + 1)
  }
  
  # Create PCA magnitude from V features
  v_columns <- grep("^V[0-9]+$", names(engineered_data), value = TRUE)
  if (length(v_columns) >= 3) {
    engineered_data$PCA_magnitude <- sqrt(rowSums(engineered_data[v_columns]^2))
  }
  
  # Create statistical features
  if (length(v_columns) > 0) {
    engineered_data$V_mean <- rowMeans(engineered_data[v_columns], na.rm = TRUE)
    engineered_data$V_std <- apply(engineered_data[v_columns], 1, sd, na.rm = TRUE)
  }
  
  # Create interaction features (that were used in training)
  if (all(c("V1", "V2") %in% names(engineered_data))) {
    engineered_data$V1_V2_interaction <- engineered_data$V1 * engineered_data$V2
  }
  
  if (all(c("V3", "V4") %in% names(engineered_data))) {
    engineered_data$V3_V4_interaction <- engineered_data$V3 * engineered_data$V4
  }
  
  if (all(c("V1", "V14") %in% names(engineered_data))) {
    engineered_data$V1_V14_interaction <- engineered_data$V1 * engineered_data$V14
  }
  
  # Create ratio features
  if (all(c("V1", "V2") %in% names(engineered_data))) {
    engineered_data$V1_V2_ratio <- ifelse(engineered_data$V2 != 0, 
                                         engineered_data$V1 / engineered_data$V2, 
                                         0)
  }
  
  return(engineered_data)
}

# Create sample data for testing
create_sample_transactions <- function(n = 20) {
  cat("Creating sample transaction data...\n")
  
  set.seed(123)
  transactions <- data.frame()
  
  for (i in 1:n) {
    transaction <- data.frame(
      Time = sample(0:1000, 1),
      V1 = rnorm(1, 0, 1.5), V2 = rnorm(1, 0, 1), V3 = rnorm(1, 0, 1),
      V4 = rnorm(1, 0, 1), V5 = rnorm(1, 0, 1), V6 = rnorm(1, 0, 1),
      V7 = rnorm(1, 0, 1), V8 = rnorm(1, 0, 1), V9 = rnorm(1, 0, 1),
      V10 = rnorm(1, 0, 1), V11 = rnorm(1, 0, 1), V12 = rnorm(1, 0, 1),
      V13 = rnorm(1, 0, 1), V14 = rnorm(1, 0, 1), V15 = rnorm(1, 0, 1),
      V16 = rnorm(1, 0, 1), V17 = rnorm(1, 0, 1), V18 = rnorm(1, 0, 1),
      V19 = rnorm(1, 0, 1), V20 = rnorm(1, 0, 1), V21 = rnorm(1, 0, 1),
      V22 = rnorm(1, 0, 1), V23 = rnorm(1, 0, 1), V24 = rnorm(1, 0, 1),
      V25 = rnorm(1, 0, 1), V26 = rnorm(1, 0, 1), V27 = rnorm(1, 0, 1),
      V28 = rnorm(1, 0, 1),
      Amount = round(runif(1, 1, 1000), 2),
      Class = sample(0:1, 1, prob = c(0.95, 0.05))  # Higher fraud rate for testing
    )
    transactions <- rbind(transactions, transaction)
  }
  
  # Save sample data
  write.csv(transactions, "sample_transactions.csv", row.names = FALSE)
  cat("✓ Created sample_transactions.csv with", n, "transactions\n")
  return(transactions)
}

# Process transactions from a file
process_transactions_file <- function(file_path = "sample_transactions.csv") {
  if (!file.exists(file_path)) {
    cat("File not found. Creating sample data...\n")
    transactions <- create_sample_transactions(20)
  } else {
    cat("Reading transactions from:", file_path, "\n")
    transactions <- read.csv(file_path)
  }
  
  cat("Processing", nrow(transactions), "transactions...\n")
  
  # Remove Time and Class columns for prediction
  transactions_for_prediction <- transactions %>% select(-Time, -Class)
  
  # Create features
  transactions_with_features <- create_features_for_prediction(transactions_for_prediction)
  
  cat("Data prepared with", ncol(transactions_with_features), "features\n")
  
  # Make predictions
  tryCatch({
    predictions <- predict(model, transactions_with_features)
    probabilities <- predict(model, transactions_with_features, type = "prob")$Fraud
    
    # Create results
    results <- data.frame(
      TransactionID = 1:nrow(transactions),
      Amount = transactions$Amount,
      ActualClass = ifelse(transactions$Class == 1, "Fraud", "Legitimate"),
      PredictedClass = as.character(predictions),
      FraudProbability = round(probabilities * 100, 2),
      RiskLevel = cut(probabilities, 
                     breaks = c(0, 0.1, 0.3, 0.5, 1),
                     labels = c("Low", "Medium", "High", "Very High"))
    )
    
    # Calculate accuracy if we have actual labels
    if ("Class" %in% names(transactions)) {
      accuracy <- mean(results$PredictedClass == results$ActualClass)
      cat("Accuracy on test data:", round(accuracy * 100, 2), "%\n")
    }
    
    # Save results
    output_file <- "fraud_detection_results.csv"
    write.csv(results, output_file, row.names = FALSE)
    
    # Display summary
    cat("\n", strrep("=", 50), "\n")
    cat("BATCH PROCESSING COMPLETE\n")
    cat(strrep("=", 50), "\n")
    cat("Total transactions processed:", nrow(results), "\n")
    cat("Fraud alerts:", sum(results$PredictedClass == "Fraud"), "\n")
    cat("High risk transactions:", sum(results$RiskLevel %in% c("High", "Very High")), "\n")
    
    if ("Class" %in% names(transactions)) {
      actual_frauds <- sum(transactions$Class == 1)
      detected_frauds <- sum(results$PredictedClass == "Fraud" & transactions$Class == 1)
      cat("Actual frauds:", actual_frauds, "\n")
      cat("Detected frauds:", detected_frauds, "\n")
      if (actual_frauds > 0) {
        cat("Detection rate:", round(detected_frauds/actual_frauds * 100, 2), "%\n")
      }
    }
    
    cat("Results saved to:", output_file, "\n")
    cat(strrep("=", 50), "\n")
    
    # Show first few results
    cat("\nFirst 5 results:\n")
    print(head(results, 5))
    
    return(results)
    
  }, error = function(e) {
    cat("❌ Prediction failed:", e$message, "\n")
    cat("Available features:", paste(names(transactions_with_features), collapse = ", "), "\n")
    return(NULL)
  })
}

# Main execution
cat("Batch Fraud Detection Demo\n")
cat("This will:\n")
cat("1. Create sample transaction data\n")
cat("2. Process transactions using the trained model\n")
cat("3. Generate fraud detection report\n\n")

results <- process_transactions_file()

if (!is.null(results)) {
  cat("\n🎉 Batch processing completed successfully!\n")
  cat("Check 'fraud_detection_results.csv' for detailed results.\n")
} else {
  cat("\n❌ Batch processing failed. Check feature compatibility.\n")
}
