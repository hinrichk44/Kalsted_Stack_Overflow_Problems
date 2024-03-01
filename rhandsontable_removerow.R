require(shiny)
require(rhandsontable)
require(shinyjs)
require(tidyverse)

cars_data <- mtcars %>%
  mutate(placement = row_number()) %>%
  relocate(placement, .before = mpg)
 


shinyApp(
  
  
  ui = fluidPage(
    useShinyjs(),
    actionButton(inputId = "remove_row", label = "Remove Row From Table"),
    rHandsontableOutput("mytable")
    
  ),
  
  
  server = function(input, output, session) {
    
    # Create a reactive data frame to store user edits
    reactive_data <- reactiveVal(cars_data)
    
    output$mytable <- renderRHandsontable({
      rhandsontable(
        data =  reactive_data(),
        selectCallback = T
      )
    })
    
    
    observeEvent(input$remove_row,{
      
      
      selected_rhands_rows <- input$mytable_select$select$r
      
      new_cars <- filter(cars_data, !(placement %in% cars_data[selected_rhands_rows, ]$placement))
      
      cars_data <- new_cars %>% mutate(placement = as.numeric(seq(1:(nrow(cars_data) - length(selected_rhands_rows)))))
  
      
    })
    
    
    observe({
      if(is.null(input$mytable_select$select$r)){
        shinyjs::disable("remove_row")
      }else{
        shinyjs::enable("remove_row")
      }
    })
    
    
  }
)