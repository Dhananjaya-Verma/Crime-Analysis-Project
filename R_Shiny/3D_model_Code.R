library(shiny)
library(plotly)

# Load the dataset
my_data <- read.csv("E:\\r_project\\r_programming.csv")

# Define UI for the app
ui <- fluidPage(
  titlePanel("3D Line Plot Visualization"),
  sidebarLayout(
    sidebarPanel(
      h4("Customize Your 3D Line Plot"),
      # Select variables for the x, y, and z axes
      selectInput("xvar", "X-axis Variable:", choices = names(my_data)),
      selectInput("yvar", "Y-axis Variable:", choices = names(my_data)),
      selectInput("zvar", "Z-axis Variable:", choices = names(my_data))
    ),
    mainPanel(
      h4("Interactive 3D Line Plot"),
      plotlyOutput("plot", height = "600px")  # Output plot with a larger display
    )
  )
)

# Define server logic for the app
server <- function(input, output) {
  output$plot <- renderPlotly({
    # Generate a 3D Line Plot
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