library(shiny)
library(rhandsontable)

ui <- fluidPage(
  titlePanel("Editable and Sortable Table Example"),
  sidebarLayout(
    sidebarPanel(
      helpText("Edit the table values in the 'Numeric Column' to sort the table."),
      actionButton("sort_button", "Sort Table")
    ),
    mainPanel(
      rHandsontableOutput("editable_table")
    )
  )
)

server <- function(input, output, session) {
  
  
  # Create an initial data frame
  # example_data <- data.frame(
  #   Name = c("John", "Alice", "Bob", "Eve"),
  #   Numeric_Column = c(5, 3, 8, 2)
  # )
  
  # Create a reactive data frame to store user edits
  # reactive_data <- reactiveVal(data)
  
  # Generating a reactive dataframe
  generate_dataframe <- reactive({
    data.frame(
      Name = c("John", "Alice", "Bob", "Eve"),
      Numeric_Column = c(5, 3, 8, 2))
  })
  
  
  
  
  
 
  
 
  # Store the reactive dataframe in reactiveValues
  rv <- reactiveValues(data = NULL)
  
  observe({
    rv$data <- generate_dataframe()
    # browser()
  })
  
 
  # Render the editable table
  output$editable_table <- renderRHandsontable({
    
    # df1 <- generate_dataframe()
    rhandsontable(rv$data)
    # reactive_data <- reactiveVal(test_data())
    
  })
  
  # Event handler for sorting the table
  observeEvent(input$sort_button, {
    edited_data <- hot_to_r(input$editable_table)
    edited_data <- edited_data[order(edited_data$Numeric_Column), ]
    rv$data <- edited_data
  })
}

shinyApp(ui, server)
