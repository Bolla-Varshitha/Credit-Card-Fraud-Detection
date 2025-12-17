# test_outputs.R
cat("=== TESTING PROJECT OUTPUTS ===\n")

# Check processed data
cat("\n1. PROCESSED DATA FILES:\n")
processed_files <- list.files("data/processed", full.names = TRUE)
if (length(processed_files) > 0) {
  for (file in processed_files) {
    cat("✓", file, "\n")
    # Try to read the file
    tryCatch({
      data <- read.csv(file, nrows = 5)
      cat("   Sample data - Dimensions:", dim(data), "\n")
    }, error = function(e) {
      cat("   ⚠ Could not read file:", e$message, "\n")
    })
  }
} else {
  cat("⚠ No files in data/processed/\n")
}

# Check model files
cat("\n2. MODEL FILES:\n")
model_files <- list.files("models", full.names = TRUE)
if (length(model_files) > 0) {
  for (file in model_files) {
    cat("✓", file, "\n")
    file_size <- file.info(file)$size
    cat("   File size:", round(file_size/1024, 2), "KB\n")
  }
} else {
  cat("⚠ No files in models/\n")
}

# Check report files
cat("\n3. REPORT FILES:\n")
report_files <- list.files("reports", full.names = TRUE)
if (length(report_files) > 0) {
  for (file in report_files) {
    cat("✓", file, "\n")
    file_size <- file.info(file)$size
    cat("   File size:", round(file_size/1024, 2), "KB\n")
  }
} else {
  cat("⚠ No files in reports/\n")
}
