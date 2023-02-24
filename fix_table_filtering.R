# Load required libraries
library(shiny)
library(tidyverse)

# Define dataframe
df <- data.frame(
  Project_Name = c("Project A", "Project B", "Project C", "Project A", "Project B", "Project C"),
  Project_Cost = c("$100,000", "$200,000", "$300,000", "$200,000", "$100,000", "$300,000"),
  Project_Duration = c("1 year", "2 years", "3 years", "2 years", "1 year", "3 years"),
  Project_Status = c("In Progress", "Completed", "Cancelled", "In Progress", "Completed", "Cancelled"),
  Project_Purpose = c("Research", "Development", "Training", "Research", "Development", "Training")
)

# Define UI
ui <- fluidPage(
  
  # Create select inputs
  selectInput("project_name", "Project Name", choices = c("All", unique(df$Project_Name))),
  selectInput("project_cost", "Project Cost", choices = c("All", unique(df$Project_Cost))),
  selectInput("project_duration", "Project Duration", choices = c("All", unique(df$Project_Duration))),
  selectInput("project_status", "Project Status", choices = c("All", unique(df$Project_Status))),
  selectInput("project_purpose", "Project Purpose", choices = c("All", unique(df$Project_Purpose))),
  
  # Create table
  tableOutput("table")
  
)

# Define server
server <- function(input, output) {
  
  # Filter dataframe based on select input values
  filtered_df <- reactive({
    df %>%
      filter(
        if (input$project_name != "All") Project_Name == input$project_name else TRUE,
        if (input$project_cost != "All") Project_Cost == input$project_cost else TRUE,
        if (input$project_duration != "All") Project_Duration == input$project_duration else TRUE,
        if (input$project_status != "All") Project_Status == input$project_status else TRUE,
        if (input$project_purpose != "All") Project_Purpose == input$project_purpose else TRUE
      )
  })
  
  # Render table based on filtered dataframe
  output$table <- renderTable({
    filtered_df()
  })
  
}

# Run the app
shinyApp(ui, server)
