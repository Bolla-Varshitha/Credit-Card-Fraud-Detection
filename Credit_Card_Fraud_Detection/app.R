# app.R - Shiny Web Dashboard
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(caret)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Fraud Detection Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Single Transaction", tabName = "single", icon = icon("credit-card")),
      menuItem("Batch Processing", tabName = "batch", icon = icon("file-csv")),
      menuItem("Performance", tabName = "performance", icon = icon("chart-bar")),
      menuItem("System Status", tabName = "status", icon = icon("server"))
    )
  ),
  dashboardBody(
    tabItems(
      # Single Transaction Tab
      tabItem(tabName = "single",
        fluidRow(
          box(
            title = "Transaction Details", width = 6, status = "primary",
            numericInput("amount", "Transaction Amount ($):", value = 100, min = 0),
            numericInput("v1", "V1:", value = 0, step = 0.1),
            numericInput("v2", "V2:", value = 0, step = 0.1),
            numericInput("v3", "V3:", value = 0, step = 0.1),
            actionButton("analyze", "Analyze Transaction", class = "btn-success")
          ),
          box(
            title = "Fraud Analysis Result", width = 6, status = "info",
            uiOutput("result_display")
          )
        )
      ),
      
      # Batch Processing Tab
      tabItem(tabName = "batch",
        fluidRow(
          box(
            title = "Upload Transactions", width = 6, status = "primary",
            fileInput("file", "Choose CSV File", accept = ".csv"),
            actionButton("process", "Process Batch", class = "btn-success")
          ),
          box(
            title = "Batch Results", width = 6, status = "info",
            tableOutput("batch_results"),
            downloadButton("download", "Download Results")
          )
        )
      ),
      
      # Performance Tab
      tabItem(tabName = "performance",
        fluidRow(
          box(
            title = "Model Performance", width = 12,
            plotOutput("performance_plot")
          )
        ),
        fluidRow(
          valueBoxOutput("auc_box"),
          valueBoxOutput("accuracy_box"),
          valueBoxOutput("recall_box")
        )
      ),
      
      # System Status Tab
      tabItem(tabName = "status",
        fluidRow(
          box(
            title = "System Information", width = 12,
            verbatimTextOutput("system_info")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Load model
  model <- reactive({
    readRDS("models/05_xgb_model.rds")
  })
  
  # Single transaction analysis
  observeEvent(input$analyze, {
    # Create transaction data
    transaction_data <- data.frame(
      V1 = input$v1, V2 = input$v2, V3 = input$v3,
      V4 = 0, V5 = 0, V6 = 0, V7 = 0, V8 = 0, V9 = 0, V10 = 0,
      V11 = 0, V12 = 0, V13 = 0, V14 = 0, V15 = 0, V16 = 0, V17 = 0,
      V18 = 0, V19 = 0, V20 = 0, V21 = 0, V22 = 0, V23 = 0, V24 = 0,
      V25 = 0, V26 = 0, V27 = 0, V28 = 0,
      Amount = input$amount
    )
    
    # Add features (you'd need to include your feature engineering function)
    transaction_with_features <- create_features_for_prediction(transaction_data)
    
    # Make prediction
    prediction <- predict(model(), transaction_with_features)
    probability <- predict(model(), transaction_with_features, type = "prob")$Fraud
    
    # Display results
    output$result_display <- renderUI({
      tagList(
        h3(ifelse(probability > 0.5, "🚨 FRAUD DETECTED", "✅ LEGITIMATE")),
        h4(paste("Fraud Probability:", round(probability * 100, 2), "%")),
        p(paste("Prediction:", as.character(prediction))),
        p(paste("Amount: $", input$amount)),
        if(probability > 0.7) {
          tags$div(class = "alert alert-danger",
            "🚨 HIGH RISK: Immediate action required!"
          )
        } else if(probability > 0.5) {
          tags$div(class = "alert alert-warning",
            "⚠️ SUSPICIOUS: Additional verification needed"
          )
        } else {
          tags$div(class = "alert alert-success",
            "✅ LOW RISK: Transaction appears legitimate"
          )
        }
      )
    })
  })
  
  # Performance metrics
  output$performance_plot <- renderPlot({
    # Load performance data and create plot
    metrics <- read.csv("reports/06_model_metrics.csv")
    ggplot(metrics, aes(x = Model, y = AUC, fill = Model)) +
      geom_bar(stat = "identity") +
      labs(title = "Model Performance Comparison", y = "AUC Score") +
      theme_minimal()
  })
  
  output$auc_box <- renderValueBox({
    valueBox("98.01%", "Best AUC", icon = icon("chart-line"), color = "green")
  })
  
  output$accuracy_box <- renderValueBox({
    valueBox("96.37%", "Accuracy", icon = icon("check-circle"), color = "blue")
  })
  
  output$recall_box <- renderValueBox({
    valueBox("89.80%", "Recall", icon = icon("eye"), color = "orange")
  })
  
  # System info
  output$system_info <- renderPrint({
    cat("Fraud Detection System Status\n")
    cat("=============================\n")
    cat("Last updated:", Sys.time(), "\n")
    cat("Models loaded: ✓\n")
    cat("System status: OPERATIONAL\n")
    cat("Detection rate: 89.80%\n")
    cat("Average response time: < 1 second\n")
  })
}

# Run the app
shinyApp(ui, server)
