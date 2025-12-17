# Deployment Guide

## 1. API Deployment
```bash
# Install required packages
Rscript -e "install.packages(c('plumber', 'caret', 'dplyr'))"

# Run API
Rscript api_deployment.R
