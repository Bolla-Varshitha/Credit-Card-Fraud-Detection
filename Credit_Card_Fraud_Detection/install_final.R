# install_final.R - Final package installation
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Core packages needed for the project
core_packages <- c(
  "dplyr", "ggplot2", "caret", "randomForest", "xgboost",
  "pROC", "recipes", "corrplot", "readr", "tidyr", "glmnet",
  "themis", "patchwork", "reshape2"
)

cat("Installing required packages for Credit Card Fraud Detection...\n")

# Install missing packages
for (pkg in core_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing:", pkg, "\n")
    install.packages(pkg, dependencies = TRUE)
  } else {
    cat("✓", pkg, "is already installed\n")
  }
}

# Verify installations
cat("\n=== VERIFYING PACKAGE INSTALLATIONS ===\n")
missing <- c()
for (pkg in core_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("✓", pkg, "loaded successfully\n")
  } else {
    cat("✗", pkg, "FAILED to load\n")
    missing <- c(missing, pkg)
  }
}

if (length(missing) == 0) {
  cat("\n🎉 ALL PACKAGES INSTALLED SUCCESSFULLY! 🎉\n")
  cat("You can now run the project with: Rscript scripts/07_main.R\n")
} else {
  cat("\n❌ Some packages failed to install:\n")
  cat(paste(missing, collapse = ", "), "\n")
  cat("Try installing them manually.\n")
}
