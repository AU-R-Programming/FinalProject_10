
library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Logistic Regression Analysis"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

          fileInput("dataset", "Upload Dataset (CSV)", accept = ".csv"),
          numericInput("alpha", "Confidence Level (alpha)", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
          numericInput("n_bootstrap", "Number of Bootstraps", value = 20, min = 1, max = 100, step = 1),
          actionButton("run", "Run Analysis")

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #  load dataset
  data <- reactive({
    req(input$dataset)
    read.csv(input$dataset$datapath)
  })

  # running the analysis
  analysis_results <- eventReactive(input$run, {
    dataset <- data()

    # Assuming predictors and response are known columns in the dataset
    X <- as.matrix(dataset[, -ncol(dataset)]) # All columns except the last
    y <- dataset[, ncol(dataset)]            # Last column is the response

    # Step 1: Compute initial beta
    beta_init <- beta_initial(X, y)

    # Step 2: Fit the model (use your fit_model function here)
    fitted_model <- fit_model(X, y, beta_init)

    # Step 3: Perform bootstrapping (use your bootstrap function here)
    bootstrap_results <- bootstrap(fitted_model, X, y, n = input$n_bootstrap)

    # Step 4: Compute metrics and confusion matrix
    metrics <- compute_metrics(fitted_model, X, y, cutoff = 0.5)
    conf_matrix <- metrics$conf_matrix

    list(
      summary = list(beta_init = beta_init, metrics = metrics),
      conf_matrix = conf_matrix,
      bootstrap = bootstrap_results
    )
  })

  # Outputs
  output$summary <- renderTable({
    req(analysis_results())
    summary <- analysis_results()$summary
    data.frame(
      Metric = names(summary$metrics),
      Value = unlist(summary$metrics)
    )
  })

  output$conf_matrix <- renderTable({
    req(analysis_results())
    analysis_results()$conf_matrix
  })

  output$metrics <- renderTable({
    req(analysis_results())
    metrics <- analysis_results()$summary$metrics
    data.frame(
      Metric = names(metrics),
      Value = unlist(metrics)
    )
  })

  output$confidence_plot <- renderPlot({
    req(analysis_results())
    bootstrap <- analysis_results()$bootstrap

    # Example: Visualize confidence intervals for each beta coefficient
    ggplot(bootstrap, aes(x = Variable, y = Estimate)) +
      geom_point() +
      geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
      labs(title = "Confidence Intervals for Beta Coefficients")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
