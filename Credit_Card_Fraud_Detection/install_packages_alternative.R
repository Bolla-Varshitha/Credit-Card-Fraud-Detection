# install_packages_alternative.R
# Multiple CRAN mirrors
cran_mirrors <- c(
  "https://cloud.r-project.org",
  "https://cran.rstudio.com",
  "https://mirror.las.iastate.edu/CRAN"
)

# Try different mirrors
for (mirror in cran_mirrors) {
  cat("Trying mirror:", mirror, "\n")
  options(repos = c(CRAN = mirror))
  
  tryCatch({
    # Test the mirror
    available.packages()
    cat("✓ Mirror", mirror, "is working\n")
    break
  }, error = function(e) {
    cat("✗ Mirror", mirror, "failed:", e$message, "\n")
  })
}

required_packages <- c(
  "dplyr", "ggplot2", "caret", "randomForest", "xgboost",
  "pROC", "ROSE", "DMwR", "recipes", "corrplot", "patchwork",
  "readr", "tidyr", "reshape2", "glmnet"
)

# Install packages one by one with error handling
for (pkg in required_packages) {
  cat("\nProcessing package:", pkg, "\n")
  
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    
    tryCatch({
      install.packages(pkg, dependencies = TRUE, quiet = FALSE)
      
      # Verify installation
      if (require(pkg, character.only = TRUE, quietly = TRUE)) {
        cat("✓ SUCCESS: Installed", pkg, "\n")
      } else {
        cat("✗ WARNING: Installation may have failed for", pkg, "\n")
      }
    }, error = function(e) {
      cat("✗ ERROR: Failed to install", pkg, ":", e$message, "\n")
    })
  } else {
    cat("✓", pkg, "is already installed\n")
  }
}

# Final verification
cat("\n=== FINAL VERIFICATION ===\n")
missing_packages <- c()
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) == 0) {
  cat("✓ All packages installed successfully!\n")
} else {
  cat("✗ The following packages failed to install:\n")
  cat(paste(missing_packages, collapse = ", "), "\n")
  cat("You may need to install them manually.\n")
}
