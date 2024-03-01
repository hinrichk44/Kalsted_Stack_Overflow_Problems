# Load required libraries
library(shiny)
library(ggplot2)
library(shinyalert)

# Define UI
ui <- fluidPage(
  useShinyalert(),
  titlePanel("Dataset Plotter"),
  sidebarLayout(
    sidebarPanel(
      actionButton("plot_button", "Toggle Plot")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive value to store which dataset's plot to display
  current_dataset <- reactiveVal("iris")
  
  # Function to generate iris plot
  iris_plot <- function() {
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point() +
      labs(title = "Iris Dataset Plot")
  }
  
  # Function to generate mtcars plot
  mtcars_plot <- function() {
    ggplot(mtcars, aes(x = mpg, y = disp)) +
      geom_point() +
      labs(title = "mtcars Dataset Plot")
  }
  
  # Observer for action button click
  observeEvent(input$plot_button, {
    shinyalert(
      title = "Change Plot",
      text = "Are you sure you want to change plots?",
      type = "warning",
      showCancelButton = TRUE,
      cancelButtonText = "No",
      confirmButtonText = "Yes",
      callbackR = function(value) {
        if (value) {
          if (current_dataset() == "iris") {
            current_dataset("mtcars")
          } else {
            current_dataset("iris")
          }
        }
      }
    )
  })
  
  # Render the plot
  output$plot <- renderPlot({
    if (current_dataset() == "iris") {
      iris_plot()
    } else {
      mtcars_plot()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
