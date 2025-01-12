library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(randomForest)
library(caret)
library(plotly)

# Load your dataset (replace 'your_data.csv' with your dataset file)
my_data <- read.csv("E:\\r_project\\r_programming.csv")

# Define UI for the app
ui <- fluidPage(
  titlePanel("Crime Analysis Portal - Crimes Against Women (2022)"),
  
  # CSS for styling the app
  tags$style(HTML("
    .card {
      padding: 20px;
      margin: 20px 0;
      background-color: #f9f9f9;
      border: 1px solid #ddd;
      border-radius: 8px;
      box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);
    }
    .logout-btn {
      position: absolute;
      top: 10px;
      right: 20px;
    }
  ")),
  
  # Layout: Navbar structure with multiple sections
  navbarPage(
    title = div(
      "Data Analysis Portal",
      actionButton("logout", "Logout", class = "btn btn-danger logout-btn")
    ),
    
    # Overview Tab
    tabPanel("Overview",
             fluidRow(
               column(12, 
                      div(class = "card",
                          h3("Welcome to the Crime Analysis Portal"),
                          p("Explore data-driven insights into crimes against women. Navigate through various interactive tabs to visualize, analyze, and model the data.")
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
    
    # ML Model Tab
    tabPanel("ML Model",
             sidebarLayout(
               sidebarPanel(
                 div(class = "card",
                     h3("Machine Learning Model Trainer"),
                     selectInput("targetVar", "Select Target Variable:", choices = c("Total_Crime_Against_Women")),
                     checkboxGroupInput("predictors", "Select Predictor Variables:", choices = c("State", "Kidnapping_Abduction_Total")),
                     actionButton("trainModel", "Train and Predict", class = "btn btn-primary")
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
    ),
    
    # 3D Line Plot Tab
    tabPanel("3D Line Plot",
             sidebarLayout(
               sidebarPanel(
                 h4("Customize Your 3D Line Plot"),
                 selectInput("xvar", "X-axis Variable:", choices = names(my_data)),
                 selectInput("yvar", "Y-axis Variable:", choices = names(my_data)),
                 selectInput("zvar", "Z-axis Variable:", choices = names(my_data))
               ),
               mainPanel(
                 h4("Interactive 3D Line Plot"),
                 plotlyOutput("plot3D", height = "600px")
               )
             )
    )
  )
)

# Define Server logic for the app
server <- function(input, output, session) {
  
  # Logout functionality
  observeEvent(input$logout, {
    session$reload()
  })
  
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
  
  # Generate 3D Line Plot
  output$plot3D <- renderPlotly({
    plot_ly(my_data, x = ~get(input$xvar), y = ~get(input$yvar), z = ~get(input$zvar), 
            type = "scatter3d", mode = "lines") %>%
      layout(scene = list(
        xaxis = list(title = input$xvar),
        yaxis = list(title = input$yvar),
        zaxis = list(title = input$zvar)
      ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)