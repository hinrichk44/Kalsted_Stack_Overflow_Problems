library(shiny)
library(rhandsontable)

# Add a checkbox column to mtcars dataset
mtcars$select_car <- rep(FALSE, nrow(mtcars))

# Define UI for application
ui <- fluidPage(
  titlePanel("mtcars Dataset with Checkbox Column"),
  mainPanel(
    rHandsontableOutput("hot")
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Define the handsontable
  output$hot <- renderRHandsontable({
    rhandsontable(mtcars, readOnly = FALSE)
  })
  
  # Update the dataset when changes are made in the handsontable
  observeEvent(input$hot, {
    mtcars <- hot_to_r(input$hot)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
