# install_packages_fixed.R
# Set CRAN mirror first
options(repos = c(CRAN = "https://cloud.r-project.org"))

required_packages <- c(
  "dplyr", "ggplot2", "caret", "randomForest", "xgboost",
  "pROC", "ROSE", "DMwR", "recipes", "corrplot", "patchwork",
  "readr", "tidyr", "reshape2", "glmnet"
)

# Function to install packages
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing package:", pkg, "\n")
    install.packages(pkg, dependencies = TRUE)
    
    # Check if installation was successful
    if (require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("✓ Successfully installed", pkg, "\n")
    } else {
      cat("✗ Failed to install", pkg, "\n")
    }
  } else {
    cat("✓", pkg, "is already installed\n")
  }
}

# Install all packages
cat("Installing required packages...\n")
for (pkg in required_packages) {
  install_if_missing(pkg)
}

cat("\n=== Package Installation Summary ===\n")
cat("All required packages should now be installed.\n")
cat("If any packages failed, they may need manual installation.\n")
