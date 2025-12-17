# ensemble_model.R
# Combine multiple models for better performance
create_ensemble <- function(models, predictions) {
  # Weighted average based on individual model performance
  weights <- c(0.4, 0.35, 0.25)  # XGBoost, RF, Logistic
  
  ensemble_prob <- (
    weights[1] * predictions$xgb_probs +
    weights[2] * predictions$rf_probs + 
    weights[3] * predictions$logistic_probs
  )
  
  return(ensemble_prob)
}
