# Required Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(randomForest)
library(caret)

# Load your dataset (replace 'your_data.csv' with your dataset file)
my_data <- read.csv("E:\\r_project\\r_programming.csv")

# Define UI for the app
ui <- fluidPage(
  titlePanel("Crime Analysis Portal - Crimes Against Women (2022)"),
  
  # Layout: Navbar structure with Overview, Visualizations, Dataset, and ML Model sections
  navbarPage("Data Analysis Portal",
             
             # Overview Tab
             tabPanel("Overview",
                      fluidRow(
                        column(12, 
                               div(class = "card",
                                   h3("Welcome to the Crime Analysis Portal"),
                              
                                   p("Welcome to our comprehensive Crime Data Analysis Portal, a dedicated resource for visualizing, analyzing, and understanding crime trends specifically related to offenses against women in 2022. This portal aims to provide accessible insights into patterns of crime, supporting communities, researchers, law enforcement, and policymakers in their mission to enhance public safety and promote justice.
"),
                                   p("Key Features:"),
                                   p("Interactive Dashboards: Explore a range of interactive dashboards that visualize data on crimes against women, including domestic violence, sexual assault, harassment, and other categories.
"),
                                   p("Data Insights by Region: View detailed statistics by state, city, and locality, with the ability to filter and compare regions to identify patterns and hotspots.
"),
                                   p("Time-Based Analysis: Understand monthly and seasonal trends in crime incidents, helping to highlight peak times and any related factors."),
                                   p("Demographic Insights: Gain insights into the demographics affected by crime, providing a deeper understanding of vulnerable groups and at-risk populations.
"),
                                   p("Our portal seeks to empower users with meaningful, data-driven insights to drive awareness and action for reducing crimes against women.")
                               )
                        )
                      )
             ),
             
             # Visualizations Tab
             tabPanel("Visualizations",
                      sidebarLayout(
                        sidebarPanel(
                          div(class = "card",
                              h3("Visualization Options"),
                              selectInput("var", "Choose a variable:", choices = names(my_data)),
                              selectInput("plotType", "Select Plot Type:", choices = c("Histogram" = "hist", "Scatter Plot" = "scatter", "Box Plot" = "box")),
                              conditionalPanel(
                                condition = "input.plotType == 'scatter'",
                                selectInput("var2", "Choose a second variable for Scatter Plot:", choices = names(my_data))
                              )
                          )
                        ),
                        mainPanel(
                          div(class = "plot-container card",
                              plotOutput("myPlot"),
                              h3("Summary Statistics"),
                              verbatimTextOutput("summary")
                          )
                        )
                      )
             ),
             
             # Dataset Tab
             tabPanel("Dataset",
                      sidebarLayout(
                        sidebarPanel(
                          div(class = "card",
                              h3("Dataset Viewer"),
                              checkboxGroupInput("columns", "Select Columns to Display:", choices = names(my_data), selected = names(my_data))
                          )
                        ),
                        mainPanel(
                          div(class = "card",
                              DT::dataTableOutput("dataTable")
                          )
                        )
                      )
             ),
             
             # ML Model Tab with Random Forest
             tabPanel("ML Model",
                      sidebarLayout(
                        sidebarPanel(
                          div(class = "card",
                              h3("Machine Learning Model Trainer"),
                              selectInput("targetVar", "Select Target Variable:", choices = c("Total_Crime_Against_Women")),
                              checkboxGroupInput("predictors", "Select Predictor Variables:", choices = c("State", "Kidnapping_Abduction_Total")),
                              actionButton("trainModel", "Train and Predict")
                          )
                        ),
                        mainPanel(
                          div(class = "card",
                              h3("Model Summary"),
                              verbatimTextOutput("modelSummary"),
                              h3("Model Accuracy (RMSE, Rsquared, MAE)"),
                              verbatimTextOutput("modelAccuracy"),
                              plotOutput("plot")
                          )
                        )
                      )
             )
  )
)

# Define Server logic for the app
server <- function(input, output) {
  
  # Generate the selected plot
  output$myPlot <- renderPlot({
    if (input$plotType == "hist") {
      ggplot(my_data, aes_string(x = input$var)) +
        geom_histogram(bins = 30, fill = "#3498db", color = "white") +
        theme_minimal() +
        labs(x = input$var, y = "Count", title = paste("Histogram of", input$var))
      
    } else if (input$plotType == "scatter") {
      ggplot(my_data, aes_string(x = input$var, y = input$var2)) +
        geom_point(color = "#3498db") +
        theme_minimal() +
        labs(x = input$var, y = input$var2, title = paste("Scatter Plot of", input$var, "vs", input$var2))
      
    } else if (input$plotType == "box") {
      ggplot(my_data, aes_string(x = input$var)) +
        geom_boxplot(fill = "#e74c3c", color = "black") +
        theme_minimal() +
        labs(x = input$var, y = "Value", title = paste("Box Plot of", input$var))
    }
  })
  
  # Generate summary statistics
  output$summary <- renderPrint({
    req(input$var)
    if (input$plotType == "scatter" && !is.null(input$var2)) {
      summary(my_data[, c(input$var, input$var2)])
    } else {
      summary(my_data[[input$var]])
    }
  })
  
  # Generate the data table with selected columns
  output$dataTable <- DT::renderDataTable({
    my_data %>% select(all_of(input$columns))
  })
  
  # Reactive event for training the model with Random Forest
  observeEvent(input$trainModel, {
    req(input$targetVar, input$predictors)
    train_data <- na.omit(my_data[, c(input$targetVar, input$predictors)])
    
    set.seed(123)
    sample_index <- sample(1:nrow(train_data), 0.8 * nrow(train_data))
    train <- train_data[sample_index, ]
    test <- train_data[-sample_index, ]
    
    # Train Random Forest Model
    rf_model <- randomForest(as.formula(paste(input$targetVar, "~", paste(input$predictors, collapse = "+"))),
                             data = train, ntree = 100)
    
    # Store the model and predictions
    output$modelSummary <- renderPrint({ rf_model })
    
    predictions <- predict(rf_model, newdata = test)
    values <- data.frame(Actual = test[[input$targetVar]], Predicted = predictions)
    
    # Calculate Accuracy
    output$modelAccuracy <- renderPrint({
      caret::postResample(pred = predictions, obs = test[[input$targetVar]])
    })
    
    # Plot Actual vs Predicted
    output$plot <- renderPlot({
      ggplot(values, aes(x = seq_along(predictions))) +
        geom_line(aes(y = Actual, color = "Actual"), size = 1, alpha = 0.8) +
        geom_line(aes(y = Predicted, color = "Predicted"), size = 1, alpha = 0.8, linetype = "dashed") +
        labs(title = "Actual vs Predicted Values", x = "Index", y = "Values") +
        theme_minimal() +
        scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue")) +
        theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
