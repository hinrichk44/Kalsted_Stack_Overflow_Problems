# Load required libraries
library(shiny)
library(tidyverse)

# Define dataframe
df <- data.frame(
  Project_ID = c(1, 2, 3, 4, 5, 6),
  Project_Name = c("Project A", "Project B", "Project C", "Project A", "Project B", "Project C"),
  Project_Cost = c("$100,000", "$200,000", "$300,000", "$200,000", "$100,000", "$300,000"),
  Project_Duration = c("1 year", "2 years", "3 years", "2 years", "1 year", "3 years"),
  Project_Status = c("In Progress", "Completed", "Cancelled", "In Progress", "Completed", "Cancelled"),
  Project_Purpose = c("Research", "Development", "Training", "Research", "Development", "Training")
)

# Define UI
ui <- fluidPage(
  
  # Create select inputs
  selectInput("project_id", "Project ID", choices = c("All", unique(df$Project_ID))),
  uiOutput("project_name_ui"),
  selectInput("project_cost", "Project Cost", choices = c("All", unique(df$Project_Cost))),
  selectInput("project_duration", "Project Duration", choices = c("All", unique(df$Project_Duration))),
  selectInput("project_status", "Project Status", choices = c("All", unique(df$Project_Status))),
  selectInput("project_purpose", "Project Purpose", choices = c("All", unique(df$Project_Purpose))),
  
  # Create table
  tableOutput("table")
  
)

# Define server
server <- function(input, output, session) {
  
  # Create reactive dataframe for filtering
  filtered_df <- reactive({
    if (!is.null(input$project_name) && input$project_name != "All") {
      df %>%
        filter(Project_Name == input$project_name)
    } else if (!is.null(input$project_id) && input$project_id != "All") {
      df %>%
        filter(Project_ID == input$project_id)
    } else {
      df %>%
        filter(
          if (input$project_cost != "All") Project_Cost %in% input$project_cost else TRUE,
          if (input$project_duration != "All") Project_Duration %in% input$project_duration else TRUE,
          if (input$project_status != "All") Project_Status %in% input$project_status else TRUE,
          if (input$project_purpose != "All") Project_Purpose %in% input$project_purpose else TRUE
        )
    }
  })
  
  # Create reactive list of project names
  project_names <- reactive({
    if (!is.null(input$project_id) && input$project_id != "All") {
      df %>%
        filter(Project_ID == input$project_id) %>%
        select(Project_Name)
    } else {
      unique(df$Project_Name)
    }
  })
  
  # Create project name select input
  output$project_name_ui <- renderUI({
    selectInput("project_name", "Project Name", choices = c("All", project_names()), selected = if (!is.null(input$project_id) && input$project_id != "All") project_names() else "All")
  })
  
  # Update project id based on project name selection
  observeEvent(input$project_name, {
    if (!is.null(input$project_name) && input$project_name != "All") {
      project_id <- df %>%
        filter(Project_Name == input$project_name) %>%
        select(Project_ID)
      updateSelectInput(session, "project_id", choices = c("All", project_id), selected = project_id)
    } else {
      updateSelectInput(session, "project_id", choices = c("All", unique(df$Project_ID)), selected = "All")
    }
  })
  
  # Render table
  output$table <- renderTable({
    filtered_df()
  })
  
}

# Run app
shinyApp(ui, server)

