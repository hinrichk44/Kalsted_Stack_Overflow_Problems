library(shiny)

# Define UI
ui <- fluidPage(
  # Create select input for dataset selection
  selectInput(inputId = "dataset",
              label = "Select a dataset:",
              choices = c("Project Cost Data", "Project Schedule Data")),
  
  # Create select input for project name selection
  selectInput(inputId = "project",
              label = "Select a project:",
              choices = NULL),
  
  # Create select input for project year selection (hidden by default)
  conditionalPanel(
    condition = "input.dataset == 'Project Schedule Data'",
    selectInput(inputId = "year",
                label = "Select a year:",
                choices = NULL)
  ),
  
  # Add a plot to display the selected dataset, project, and year
  plotOutput(outputId = "plot")
)

# Define server
server <- function(input, output, session) {
  
  # Create reactive dataset based on the selected dataset input
  data <- reactive({
    if (input$dataset == "Project Cost Data") {
      # Return Project Cost Data
      # Replace this with code to load your own dataset
      data <- data.frame(Project = c("Project A", "Project B", "Project C"),
                         Year = c(2020, 2021, 2022),
                         Cost = c(10000, 15000, 20000))
    } else {
      # Return Project Schedule Data
      # Replace this with code to load your own dataset
      data <- data.frame(Project = c("Project A", "Project B", "Project C"),
                         Year = c(2020, 2021, 2022),
                         Schedule = c(10, 15, 20))
    }
    return(data)
  })
  
  # Update project names based on the selected dataset input
  observe({
    updateSelectInput(session,
                      inputId = "project",
                      label = "Select a project:",
                      choices = unique(data()$Project))
  })
  
  # Update years based on the selected project input (only for Project Schedule Data)
  # observe({
  #   if (input$dataset == "Project Schedule Data") {
  #     updateSelectInput(session,
  #                       inputId = "year",
  #                       label = "Select a year:",
  #                       choices = unique(data()[data()$Project == input$project, "Year"]))
  #   }
  # })
  
  # Create plot based on the selected dataset, project, and year inputs
  output$plot <- renderPlot({
    plotdata <- data()[data()$Project == input$project & data()$Year == input$year, ]
    if (input$dataset == "Project Cost Data") {
      plot(x = plotdata$Year,
           y = plotdata$Cost,
           xlab = "Year",
           ylab = "Cost",
           main = paste("Project", input$project, "Cost in", input$year))
    } else {
      plot(x = plotdata$Year,
           y = plotdata$Schedule,
           xlab = "Year",
           ylab = "Schedule",
           main = paste("Project", input$project, "Schedule in", input$year))
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
