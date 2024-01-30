library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Action Button Confirmation"),
  sidebarLayout(
    sidebarPanel(
      actionButton("runButton", "Run Action")
    ),
    mainPanel(
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$runButton, {
    showModal(
      modalDialog(
        title = "Confirmation",
        "Are you sure you want to run the action?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirmRun", "Confirm")
        )
      )
    )
  })
  
  observeEvent(input$confirmRun, {
    # This code will run when the "Confirm" button in the modal is clicked
    # You can place your action code here
    # For demonstration purposes, we'll just print a message
    print("Action is confirmed and will be executed.")
    removeModal() # Close the modal dialog
  })
  
  
  
}

shinyApp(ui, server)
