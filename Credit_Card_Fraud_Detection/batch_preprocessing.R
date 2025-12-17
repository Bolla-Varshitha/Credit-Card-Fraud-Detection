# batch_processing.R
process_transactions_batch <- function(transactions_file) {
  # Load model
  model <- readRDS("models/05_xgb_model.rds")
  
  # Load new transactions
  new_transactions <- read.csv(transactions_file)
  
  # Add engineered features (same as in training)
  new_transactions <- create_features(new_transactions)
  
  # Make predictions
  predictions <- predict(model, new_transactions)
  probabilities <- predict(model, new_transactions, type = "prob")$Fraud
  
  # Create results
  results <- data.frame(
    transaction_id = 1:nrow(new_transactions),
    prediction = predictions,
    fraud_probability = probabilities,
    status = ifelse(probabilities > 0.5, "FRAUD", "LEGITIMATE")
  )
  
  # Save results
  write.csv(results, "fraud_predictions.csv", row.names = FALSE)
  
  # Summary
  fraud_count <- sum(results$status == "FRAUD")
  cat("Batch Processing Complete:\n")
  cat("Total transactions:", nrow(results), "\n")
  cat("Fraudulent transactions:", fraud_count, "\n")
  cat("Fraud rate:", round(fraud_count/nrow(results)*100, 2), "%\n")
  
  return(results)
}

# Usage
# results <- process_transactions_batch("new_transactions.csv")
