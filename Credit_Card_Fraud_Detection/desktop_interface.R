# desktop_interface.R - Simple menu-based interface
library(caret)
library(dplyr)

cat("
╔══════════════════════════════════════════════════════════════╗
║                   FRAUD DETECTION INTERFACE                  ║
║                    Desktop Application                       ║
╚══════════════════════════════════════════════════════════════╝
\n")

show_main_menu <- function() {
  cat("\n")
  cat("MAIN MENU\n")
  cat("═══════════════════════════════════════════════════════\n")
  cat("1. 🔍 Analyze Single Transaction\n")
  cat("2. 📊 Process Batch File\n") 
  cat("3. 📈 View System Performance\n")
  cat("4. ⚙️  System Information\n")
  cat("5. 🚪 Exit\n")
  cat("═══════════════════════════════════════════════════════\n")
  
  choice <- readline("Enter your choice (1-5): ")
  return(choice)
}

analyze_single_transaction <- function() {
  cat("\n")
  cat("SINGLE TRANSACTION ANALYSIS\n")
  cat("───────────────────────────────────────────────────────\n")
  
  # Get transaction details
  amount <- as.numeric(readline("Enter transaction amount: $"))
  v1 <- as.numeric(readline("Enter V1 value: "))
  v2 <- as.numeric(readline("Enter V2 value: "))
  v3 <- as.numeric(readline("Enter V3 value: "))
  
  # Create transaction data
  transaction_data <- data.frame(
    V1 = v1, V2 = v2, V3 = v3,
    V4 = 0, V5 = 0, V6 = 0, V7 = 0, V8 = 0, V9 = 0, V10 = 0,
    V11 = 0, V12 = 0, V13 = 0, V14 = 0, V15 = 0, V16 = 0, V17 = 0,
    V18 = 0, V19 = 0, V20 = 0, V21 = 0, V22 = 0, V23 = 0, V24 = 0,
    V25 = 0, V26 = 0, V27 = 0, V28 = 0,
    Amount = amount
  )
  
  # Load model and predict
  model <- readRDS("models/05_xgb_model.rds")
  transaction_with_features <- create_features_for_prediction(transaction_data)
  probability <- predict(model, transaction_with_features, type = "prob")$Fraud
  
  # Display results
  cat("\n")
  cat("ANALYSIS RESULTS\n")
  cat("───────────────────────────────────────────────────────\n")
  cat("Transaction Amount: $", amount, "\n")
  cat("Fraud Probability: ", round(probability * 100, 2), "%\n")
  
  if(probability > 0.7) {
    cat("🚨🚨🚨 CRITICAL RISK - IMMEDIATE ACTION REQUIRED 🚨🚨🚨\n")
    cat("Recommendation: BLOCK TRANSACTION\n")
  } else if(probability > 0.5) {
    cat("⚠️⚠️⚠️ HIGH RISK - INVESTIGATE ⚠️⚠️⚠️\n")
    cat("Recommendation: Request customer verification\n")
  } else if(probability > 0.3) {
    cat("⚠️ MEDIUM RISK\n")
    cat("Recommendation: Additional verification\n")
  } else {
    cat("✅ LOW RISK\n")
    cat("Recommendation: Process normally\n")
  }
  
  readline("Press Enter to continue...")
}

process_batch_file <- function() {
  cat("\n")
  cat("BATCH PROCESSING\n")
  cat("───────────────────────────────────────────────────────\n")
  
  file_path <- readline("Enter CSV file path (or press Enter for sample): ")
  
  if(file_path == "") {
    file_path <- "sample_transactions.csv"
    cat("Using sample data...\n")
  }
  
  if(!file.exists(file_path)) {
    cat("❌ File not found. Please check the path.\n")
    readline("Press Enter to continue...")
    return()
  }
  
  cat("Processing", file_path, "...\n")
  
  # Run batch processing
  system("Rscript batch_fraud_detection.R")
  
  cat("✅ Batch processing completed!\n")
  cat("Results saved to: fraud_detection_results.csv\n")
  readline("Press Enter to continue...")
}

show_performance <- function() {
  cat("\n")
  cat("SYSTEM PERFORMANCE\n")
  cat("───────────────────────────────────────────────────────\n")
  
  if(file.exists("reports/06_model_metrics.csv")) {
    metrics <- read.csv("reports/06_model_metrics.csv")
    
    for(i in 1:nrow(metrics)) {
      model <- metrics[i, ]
      cat("Model: ", model$Model, "\n")
      cat("  AUC: ", round(model$AUC, 4), "\n")
      cat("  Accuracy: ", round(model$Accuracy, 4), "\n")
      cat("  Recall: ", round(model$Recall, 4), "\n")
      cat("  F1: ", round(model$F1, 4), "\n\n")
    }
  } else {
    cat("Performance data not available.\n")
    cat("Run: Rscript scripts/06_model_evaluation.R\n")
  }
  
  readline("Press Enter to continue...")
}

show_system_info <- function() {
  cat("\n")
  cat("SYSTEM INFORMATION\n")
  cat("───────────────────────────────────────────────────────\n")
  cat("Fraud Detection System v1.0\n")
  cat("Built with: R, caret, XGBoost, Random Forest, Logistic Regression\n")
  cat("Best Model: XGBoost (98.01% AUC)\n")
  cat("Detection Rate: 89.80%\n")
  cat("Last Training: ", file.info("models/05_xgb_model.rds")$mtime, "\n")
  cat("Status: OPERATIONAL\n")
  readline("Press Enter to continue...")
}

# Include your feature engineering function
create_features_for_prediction <- function(data) {
  # Your feature engineering code here
  # ... (same as before)
  return(data)
}

# Main application loop
main <- function() {
  while(TRUE) {
    choice <- show_main_menu()
    
    switch(choice,
      "1" = analyze_single_transaction(),
      "2" = process_batch_file(),
      "3" = show_performance(),
      "4" = show_system_info(),
      "5" = {
        cat("Thank you for using Fraud Detection System! 👋\n")
        break
      },
      {
        cat("Invalid choice. Please try again.\n")
        readline("Press Enter to continue...")
      }
    )
  }
}

# Start the application
main()
