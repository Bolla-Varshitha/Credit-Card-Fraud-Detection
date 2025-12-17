# install_deployment_packages.R
cat("Installing deployment packages...\n")

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Packages needed for deployment
deployment_packages <- c(
  "plumber",    # API framework
  "jsonlite",   # JSON handling
  "httr",       # HTTP requests
  "config",     # Configuration management
  "logger"      # Logging
)

# Install packages
for (pkg in deployment_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing:", pkg, "\n")
    install.packages(pkg, dependencies = TRUE)
  } else {
    cat("✓", pkg, "is already installed\n")
  }
}

cat("\n✅ Package installation completed!\n")
