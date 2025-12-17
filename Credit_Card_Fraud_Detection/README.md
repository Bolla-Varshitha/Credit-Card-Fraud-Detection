# Credit Card Fraud Detection Project

A comprehensive machine learning project for detecting fraudulent credit card transactions using R.

## Project Structure

credit-card-fraud-detection/
├── data/ # Data directory
├── scripts/ # R scripts for the pipeline
├── models/ # Saved trained models
├── reports/ # Evaluation reports and plots
├── config/ # Configuration files
└── README.md # Project documentation

## Installation and Setup

1. Clone this repository
2. Install required R packages:
```r
install.packages(c("dplyr", "ggplot2", "caret", "randomForest", "xgboost", 
                   "pROC", "ROSE", "DMwR", "recipes", "corrplot", "patchwork"))
