# debug_project.R
cat("=== DEBUGGING CREDIT CARD FRAUD DETECTION PROJECT ===\n")

# Check current working directory
cat("Current working directory:", getwd(), "\n")

# Check if directories exist
cat("\n=== CHECKING DIRECTORIES ===\n")
dirs <- c("data/raw", "data/processed", "models", "reports")
for (dir in dirs) {
  if (dir.exists(dir)) {
    cat("✓ Directory exists:", dir, "\n")
    files <- list.files(dir, recursive = TRUE)
    if (length(files) > 0) {
      cat("  Files:", paste(files, collapse = ", "), "\n")
    } else {
      cat("  ⚠ Directory is empty\n")
    }
  } else {
    cat("✗ Directory missing:", dir, "\n")
  }
}

# Check if data file exists
cat("\n=== CHECKING DATA FILE ===\n")
if (file.exists("data/raw/creditcard.csv")) {
  cat("✓ Data file exists: data/raw/creditcard.csv\n")
  # Check file size
  file_info <- file.info("data/raw/creditcard.csv")
  cat("  File size:", round(file_info$size / 1024 / 1024, 2), "MB\n")
} else {
  cat("✗ Data file missing: data/raw/creditcard.csv\n")
}

# Check if scripts can run individually
cat("\n=== TESTING INDIVIDUAL SCRIPTS ===\n")
scripts <- c(
  "scripts/01_data_loading.R",
  "scripts/02_data_preprocessing.R", 
  "scripts/03_exploratory_analysis.R"
)

for (script in scripts) {
  if (file.exists(script)) {
    cat("✓ Script exists:", script, "\n")
  } else {
    cat("✗ Script missing:", script, "\n")
  }
}
