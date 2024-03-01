library(shiny)
library(rhandsontable)
library(shinyjs)
library(dplyr)

cars_data <- mtcars |>
  mutate(placement = row_number())  |>
  relocate(placement, .before = mpg)

shinyApp(
  ui = fluidPage(
    useShinyjs(),
    actionButton(inputId = "remove_row", label = "Remove Row From Table", disabled = ''),
    rHandsontableOutput("mytable")
  ),
  
  server = function(input, output, session) {
    rv <- reactiveValues(df = cars_data)
    
    output$mytable <- renderRHandsontable({
      rhandsontable(data = rv$df,
                    selectCallback = TRUE)
    })
    
    observe({
      if (!is.null(input$mytable_select$select$r)) {
        shinyjs::enable("remove_row")
      }
    })
    
    observeEvent(input$remove_row, {
      selected_rhands_rows <- input$mytable_select$select$r
      
      rv$df <- rv$df |>
        slice(-c(selected_rhands_rows)) |>
        mutate(placement = row_number())
      
      output$mytable <- renderRHandsontable({
        rhandsontable(data = rv$df,
                      selectCallback = TRUE)
      })
      
      shinyjs::disable("remove_row")
    })
  }
)
