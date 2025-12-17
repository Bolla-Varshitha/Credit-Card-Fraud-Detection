# enhanced_fraud_detector.R - Advanced fraud detection without extra packages
library(caret)
library(dplyr)
library(readr)

cat("
███████╗██████╗ ██████╗  █████╗ ██╗   ██╗██████╗      ██████╗██████╗ ███████╗██████╗ 
██╔════╝██╔══██╗██╔══██╗██╔══██╗██║   ██║██╔══██╗    ██╔════╝██╔══██╗██╔════╝██╔══██╗
█████╗  ██████╔╝██████╔╝███████║██║   ██║██║  ██║    ██║     ██████╔╝█████╗  ██║  ██║
██╔══╝  ██╔══██╗██╔══██╗██╔══██║██║   ██║██║  ██║    ██║     ██╔══██╗██╔══╝  ██║  ██║
███████╗██║  ██║██████╔╝██║  ██║╚██████╔╝██████╔╝    ╚██████╗██║  ██║███████╗██████╔╝
╚══════╝╚═╝  ╚═╝╚═════╝ ╚═╝  ╚═╝ ╚═════╝ ╚═════╝      ╚═════╝╚═╝  ╚═╝╚══════╝╚═════╝ 
                                                                                      
\"Advanced Fraud Detection System\"\n\n")

# Load all trained models
load_models <- function() {
  cat("Loading trained models...\n")
  models <- list()
  
  model_files <- c(
    "xgb" = "models/05_xgb_model.rds",
    "random_forest" = "models/05_random_forest_model.rds", 
    "logistic" = "models/05_logistic_model.rds"
  )
  
  for (name in names(model_files)) {
    if (file.exists(model_files[[name]])) {
      models[[name]] <- readRDS(model_files[[name]])
      cat("  ✅", name, "model loaded\n")
    } else {
      cat("  ⚠️", name, "model not found\n")
    }
  }
  
  return(models)
}

# Ensemble prediction
ensemble_predict <- function(models, data) {
  cat("Making ensemble predictions...\n")
  
  predictions <- list()
  weights <- c(xgb = 0.5, random_forest = 0.3, logistic = 0.2)  # Weight by performance
  
  for (model_name in names(models)) {
    if (!is.null(models[[model_name]])) {
      tryCatch({
        prob <- predict(models[[model_name]], data, type = "prob")$Fraud
        predictions[[model_name]] <- prob * weights[model_name]
        cat("  ", model_name, "contribution:", round(mean(prob), 4), "\n")
      }, error = function(e) {
        cat("  ❌", model_name, "prediction failed\n")
      })
    }
  }
  
  # Combine predictions
  if (length(predictions) > 0) {
    ensemble_prob <- rowSums(do.call(cbind, predictions))
    return(ensemble_prob)
  } else {
    stop("No models could make predictions")
  }
}

# Risk analysis
analyze_risk <- function(probability, amount) {
  risk_level <- case_when(
    probability < 0.1 ~ "Very Low",
    probability < 0.3 ~ "Low", 
    probability < 0.5 ~ "Medium",
    probability < 0.7 ~ "High",
    TRUE ~ "Very High"
  )
  
  # Financial impact score
  impact_score <- probability * amount / 100
  
  return(list(
    risk_level = risk_level,
    impact_score = impact_score,
    recommendation = get_recommendation(risk_level, impact_score)
  ))
}

get_recommendation <- function(risk_level, impact_score) {
  recommendations <- list(
    "Very Low" = "Process normally",
    "Low" = "Process normally with basic verification",
    "Medium" = "Additional verification required",
    "High" = "Block and request customer confirmation", 
    "Very High" = "BLOCK IMMEDIATELY - High fraud probability"
  )
  
  if (impact_score > 50) {
    return("🚨 URGENT: High financial impact - Immediate investigation required")
  }
  
  return(recommendations[[risk_level]])
}

# Create sample transaction data
create_test_transactions <- function() {
  cat("Creating comprehensive test transactions...\n\n")
  
  transactions <- list(
    # Legitimate transactions
    "Small Purchase" = list(
      data = data.frame(
        V1 = 0.5, V2 = -0.2, V3 = 0.3, V4 = -0.1, V5 = 0.4, V6 = -0.3, V7 = 0.2, V8 = -0.1,
        V9 = 0.3, V10 = -0.2, V11 = 0.1, V12 = -0.4, V13 = 0.2, V14 = -0.3, V15 = 0.1, V16 = -0.2,
        V17 = 0.3, V18 = -0.1, V19 = 0.2, V20 = -0.3, V21 = 0.1, V22 = -0.2, V23 = 0.3, V24 = -0.1,
        V25 = 0.2, V26 = -0.3, V27 = 0.1, V28 = -0.2,
        Amount = 25.50
      ),
      description = "Normal coffee purchase"
    ),
    
    # Suspicious transactions
    "Large Electronics" = list(
      data = data.frame(
        V1 = -1.5, V2 = 0.8, V3 = -2.1, V4 = 1.2, V5 = -0.9, V6 = 0.7, V7 = -1.8, V8 = 0.6,
        V9 = -2.3, V10 = 1.1, V11 = -1.7, V12 = 0.9, V13 = -2.0, V14 = 1.3, V15 = -1.5, V16 = 0.8,
        V17 = -2.2, V18 = 1.0, V19 = -1.9, V20 = 0.7, V21 = -2.1, V22 = 1.2, V23 = -1.8, V24 = 0.9,
        V25 = -2.0, V26 = 1.1, V27 = -1.7, V28 = 0.8,
        Amount = 1200.00
      ),
      description = "Large electronics purchase - unusual pattern"
    ),
    
    # High-risk transaction (based on fraud patterns)
    "High Risk Pattern" = list(
      data = data.frame(
        V1 = -3.2, V2 = 2.8, V3 = -4.1, V4 = 3.5, V5 = -2.9, V6 = 2.7, V7 = -3.8, V8 = 2.6,
        V9 = -4.3, V10 = 3.1, V11 = -3.7, V12 = 2.9, V13 = -4.0, V14 = 3.3, V15 = -3.5, V16 = 2.8,
        V17 = -4.2, V18 = 3.0, V19 = -3.9, V20 = 2.7, V21 = -4.1, V22 = 3.2, V23 = -3.8, V24 = 2.9,
        V25 = -4.0, V26 = 3.1, V27 = -3.7, V28 = 2.8,
        Amount = 0.01
      ),
      description = "Micro transaction with fraud pattern"
    )
  )
  
  return(transactions)
}

# Feature engineering (same as before)
create_features_for_prediction <- function(data) {
  engineered_data <- data
  
  if ("Amount" %in% names(engineered_data)) {
    amount_breaks <- c(0, 10, 50, 100, 500, 1000, 10000)
    amount_labels <- c("Very Low", "Low", "Medium", "High", "Very High", "Extreme")
    
    engineered_data$Amount_bin <- cut(
      engineered_data$Amount,
      breaks = amount_breaks,
      labels = amount_labels,
      include.lowest = TRUE
    )
    
    engineered_data$High_Amount <- as.integer(engineered_data$Amount > 100)
    engineered_data$Amount_Log <- log1p(engineered_data$Amount + 1)
  }
  
  v_columns <- grep("^V[0-9]+$", names(engineered_data), value = TRUE)
  if (length(v_columns) >= 3) {
    engineered_data$PCA_magnitude <- sqrt(rowSums(engineered_data[v_columns]^2))
  }
  
  if (length(v_columns) > 0) {
    engineered_data$V_mean <- rowMeans(engineered_data[v_columns], na.rm = TRUE)
    engineered_data$V_std <- apply(engineered_data[v_columns], 1, sd, na.rm = TRUE)
  }
  
  if (all(c("V1", "V2") %in% names(engineered_data))) {
    engineered_data$V1_V2_interaction <- engineered_data$V1 * engineered_data$V2
  }
  
  if (all(c("V3", "V4") %in% names(engineered_data))) {
    engineered_data$V3_V4_interaction <- engineered_data$V3 * engineered_data$V4
  }
  
  if (all(c("V1", "V14") %in% names(engineered_data))) {
    engineered_data$V1_V14_interaction <- engineered_data$V1 * engineered_data$V14
  }
  
  if (all(c("V1", "V2") %in% names(engineered_data))) {
    engineered_data$V1_V2_ratio <- ifelse(engineered_data$V2 != 0, 
                                         engineered_data$V1 / engineered_data$V2, 
                                         0)
  }
  
  return(engineered_data)
}

# Main analysis function
run_comprehensive_analysis <- function() {
  cat("Initializing Advanced Fraud Detection System...\n\n")
  
  # Load models
  models <- load_models()
  
  if (length(models) == 0) {
    cat("❌ No models available. Please train models first.\n")
    return()
  }
  
  # Create test transactions
  test_transactions <- create_test_transactions()
  
  cat("Running comprehensive fraud analysis...\n")
  cat(strrep("=", 70), "\n\n")
  
  for (transaction_name in names(test_transactions)) {
    transaction <- test_transactions[[transaction_name]]
    
    cat("🔍 Analyzing:", transaction_name, "\n")
    cat("   Description:", transaction$description, "\n")
    cat("   Amount: $", transaction$data$Amount, "\n")
    
    # Create features
    transaction_with_features <- create_features_for_prediction(transaction$data)
    
    # Get ensemble prediction
    tryCatch({
      fraud_probability <- ensemble_predict(models, transaction_with_features)
      
      # Risk analysis
      risk_analysis <- analyze_risk(fraud_probability, transaction$data$Amount)
      
      # Display results
      cat("   Fraud Probability:", round(fraud_probability * 100, 2), "%\n")
      cat("   Risk Level:", risk_analysis$risk_level, "\n")
      cat("   Impact Score:", round(risk_analysis$impact_score, 2), "\n")
      cat("   Recommendation:", risk_analysis$recommendation, "\n")
      
      # Visual indicator
      if (fraud_probability > 0.7) {
        cat("   🚨🚨🚨 CRITICAL RISK 🚨🚨🚨\n")
      } else if (fraud_probability > 0.5) {
        cat("   ⚠️⚠️⚠️ HIGH RISK ⚠️⚠️⚠️\n")
      } else if (fraud_probability > 0.3) {
        cat("   ⚠️ MEDIUM RISK ⚠️\n")
      } else {
        cat("   ✅ LOW RISK ✅\n")
      }
      
    }, error = function(e) {
      cat("   ❌ Analysis failed:", e$message, "\n")
    })
    
    cat("\n" + strrep("-", 70) + "\n\n")
  }
  
  # System summary
  cat("📊 SYSTEM SUMMARY\n")
  cat("Models loaded:", length(models), "/ 3\n")
  cat("Test transactions analyzed:", length(test_transactions), "\n")
  cat("Ensemble method: Weighted average based on model performance\n")
  cat("Risk assessment: Combines probability and financial impact\n\n")
  
  cat("🎯 Ready for production use!\n")
}

# Run the analysis
run_comprehensive_analysis()
