# Define UI for application 
ui <- fluidPage(
  titlePanel("Multiply SelectInputs"),
  sidebarLayout(
    sidebarPanel(
      tags$style(".sidebar .well {width: auto;}"),
      fluidRow(
        column(4, selectInput("select1", "Select 1:", choices = 1:5)),
        column(4, selectInput("select2", "Select 2:", choices = 6:10)),
        column(4, 
               div(style="display: flex; align-items: center;",
                   h4(style="margin-right: 10px;", "="), 
                   selectInput("result", "", choices = NULL)
               )
        )
      )
    ),
    mainPanel(
      # Output plot
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Update the result selectInput based on the other inputs
  observe({
    selected1 <- as.numeric(input$select1)
    selected2 <- as.numeric(input$select2)
    if (!is.na(selected1) && !is.na(selected2)) {
      result <- selected1 * selected2
      updateSelectInput(session, "result", choices = result)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
