# business_impact.R
# Calculate financial impact of your fraud detection system
calculate_savings <- function() {
  avg_fraud_amount <- 500  # Average fraudulent transaction
  detection_rate <- 0.898  # Your model's recall
  monthly_transactions <- 100000
  fraud_rate <- 0.0015     # Typical fraud rate (0.15%)
  
  monthly_frauds <- monthly_transactions * fraud_rate
  detected_frauds <- monthly_frauds * detection_rate
  monthly_savings <- detected_frauds * avg_fraud_amount
  
  cat(sprintf("Monthly fraud prevention: $%s\n", 
              format(monthly_savings, big.mark = ",")))
  cat(sprintf("Annual savings: $%s\n", 
              format(monthly_savings * 12, big.mark = ",")))
}
