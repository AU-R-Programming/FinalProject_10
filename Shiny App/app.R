library(shiny)
library(Estimater)

# Define UI
ui <- fluidPage(
  titlePanel("Logistic Regression Shiny App"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("response_selector"),
      uiOutput("predictors_selector"),
      numericInput("B", "Number of Bootstraps:", value = 50, min = 1),
      numericInput("alpha", "Significance Level (alpha):", value = 0.05, min = 0.01, max = 0.1),
      actionButton("run", "Run Analysis")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Results",
                 h4("Optimized Coefficients"),
                 verbatimTextOutput("beta_hat"),
                 h4("Bootstrap Confidence Intervals"),
                 verbatimTextOutput("confidence_intervals"),
                 h4("Performance Metrics"),
                 verbatimTextOutput("metrics")),
        tabPanel("Visualization",
                 plotOutput("confusion_matrix_plot"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive expression to load and preprocess the dataset
  dataset <- reactive({
    req(input$file)
    data <- read.csv(input$file$datapath, sep = ";", quote = '"')  # Handle semicolon-delimited files
    colnames(data) <- trimws(make.names(colnames(data)))  # Clean column names
    data
  })

  # Dynamically generate response variable selector
  output$response_selector <- renderUI({
    req(dataset())
    selectInput("response", "Select Response Variable:", choices = colnames(dataset()))
  })

  # Dynamically generate predictor variable selector
  output$predictors_selector <- renderUI({
    req(dataset())
    checkboxGroupInput("predictors", "Select Predictor Variables:", choices = colnames(dataset()))
  })

  observeEvent(input$run, {
    req(dataset(), input$response, input$predictors)

    # Extract selected predictors and response
    data <- dataset()

    # Ensure predictors are numeric
    predictors <- data[, input$predictors, drop = FALSE]
    X <- as.matrix(dplyr::select_if(predictors, is.numeric))  # Keep only numeric predictors
    X <- cbind(1, X)  # Add intercept

    # Convert response variable to numeric
    y <- as.numeric(as.factor(data[[input$response]])) - 1  # Convert factor to binary (0/1)

    # Check for errors
    if (ncol(X) == 0 || any(is.na(y))) {
      showNotification("Error: Ensure predictors are numeric and the response variable is binary.", type = "error")
      return(NULL)
    }

    # Fit logistic regression and compute results
    results <- logistic_regression_pipeline(n = nrow(X), lambda = 0, beta_true = NULL, B = input$B, alpha = input$alpha)

    # Display results
    output$beta_hat <- renderPrint({
      results$beta_hat
    })

    output$confidence_intervals <- renderPrint({
      results$confidence_interval
    })

    output$metrics <- renderPrint({
      results$metrics
    })

    # Visualization: Confusion matrix plot
    output$confusion_matrix_plot <- renderPlot({
      cm <- results$metrics$confusion_matrix
      barplot(cm, beside = TRUE, legend = TRUE, col = c("blue", "red"),
              main = "Confusion Matrix", xlab = "Actual", ylab = "Predicted")
    })
  })
}


# Run the app
shinyApp(ui = ui, server = server)
