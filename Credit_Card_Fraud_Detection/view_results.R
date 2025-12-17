# view_results.R - View project results
cat("=== CREDIT CARD FRAUD DETECTION - PROJECT RESULTS ===\n\n")

# Check directories
cat("1. PROCESSED DATA FILES:\n")
processed_files <- list.files("data/processed", full.names = TRUE)
for (file in processed_files) {
  file_size <- file.info(file)$size
  cat("  ✓", basename(file), "-", round(file_size/1024, 1), "KB\n")
}

cat("\n2. TRAINED MODELS:\n")
model_files <- list.files("models", full.names = TRUE)
for (file in model_files) {
  file_size <- file.info(file)$size
  cat("  ✓", basename(file), "-", round(file_size/1024, 1), "KB\n")
}

cat("\n3. REPORTS AND PLOTS:\n")
report_files <- list.files("reports", full.names = TRUE)
for (file in report_files) {
  file_size <- file.info(file)$size
  cat("  ✓", basename(file), "-", round(file_size/1024, 1), "KB\n")
}

# Show model performance
cat("\n4. MODEL PERFORMANCE SUMMARY:\n")
metrics_file <- "reports/06_model_metrics.csv"
if (file.exists(metrics_file)) {
  metrics <- read.csv(metrics_file)
  print(metrics)
  
  # Best model
  best_model <- metrics[which.max(metrics$F1), ]
  cat("\n🏆 BEST MODEL:", best_model$Model, "\n")
  cat("   F1 Score:", round(best_model$F1, 4), "\n")
  cat("   AUC:", round(best_model$AUC, 4), "\n")
  cat("   Accuracy:", round(best_model$Accuracy, 4), "\n")
  cat("   Precision:", round(best_model$Precision, 4), "\n")
  cat("   Recall:", round(best_model$Recall, 4), "\n")
}

cat("\n✅ PROJECT COMPLETED SUCCESSFULLY!\n")
cat("All outputs have been generated in their respective folders.\n")
