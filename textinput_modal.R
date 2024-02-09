library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(), # <--- don't forget that!!  
  titlePanel("Example Shiny App"),
  sidebarLayout(
    sidebarPanel(
      actionButton("add_data_btn", "Add Data"),
      actionButton("edit_data_btn", "Edit Data")
    ),
    mainPanel(
      textOutput("text_output"),
      textOutput("numeric_output")
    )
  )
)


server <- function(input, output, session) {
  
  saved_text <- reactiveVal(NULL)
  saved_numeric <- reactiveVal(NULL)
  
  observeEvent(input$add_data_btn, {
    showModal(modalDialog(
      textInput(inputId = "text_input", label = "Enter Text"),
      numericInput(inputId = "numeric_input", label = "Enter Number 1-10", value = 1, min = 1, max = 10),
      footer = tagList(
        actionButton("save_btn", "Save")
      )
    ))
  })
  
  
  observeEvent(input$save_btn, {
    saved_text(input$text_input)
    saved_numeric(input$numeric_input)
    removeModal()
  })
  
  
  observeEvent(input$edit_data_btn, {
    showModal(modalDialog(
      disabled(textInput("text_input", "Enter Text", value = saved_text())),
      numericInput("numeric_input", "Enter Number 1-10", value = saved_numeric(),  min = 1, max = 10),
      footer = tagList(
        actionButton("update_btn", "Update Data")
      )
    ))
  })
  
  
  observeEvent(input$update_btn, {
    saved_numeric(input$numeric_input)
    removeModal()
  })
  
  
  output$text_output <- renderText({
    saved_text()
  })
  
  
  output$numeric_output <- renderText({
    saved_numeric()
  })
}

shinyApp(ui, server)