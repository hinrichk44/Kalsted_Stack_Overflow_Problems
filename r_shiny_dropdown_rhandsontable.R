library(shiny)
library(rhandsontable)
library(dplyr)

# Define the tibbles
projects <- tibble(
  `Project ID` = c("P001", "P002", "P001", "P002"),
  `Project APPN` = c("AAA", "AAA", "BBB", "BBB")
)

project_types <- tibble(
  `Project ID` = c("P001", "P001", "P002", "P002"),
  `Project Types` = c("Pool", "House", "Apartment", "School")
)

# Define UI
ui <- fluidPage(
  titlePanel("Projects rhandsontable"),
  mainPanel(
    rHandsontableOutput("projects_table")
  )
)

# Define server logic
server <- function(input, output) {
  output$projects_table <- renderRHandsontable({
    if (is.null(input$projects_table)) {
      df <- projects
      # Add an empty Project Types column
      df$`Project Types` <- NA
    } else {
      df <- hot_to_r(input$projects_table)
    }
    
    # Create a unique list of Project Types for the dropdown
    unique_project_types <- unique(project_types$`Project Types`)
    
    rhandsontable(df) %>%
      hot_col("Project Types", type = "dropdown", source = unique_project_types)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
