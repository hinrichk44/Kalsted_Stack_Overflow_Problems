library(shiny)
library(rhandsontable)
library(shinyjs)
library(dplyr)



cars_data <- mtcars %>%
  mutate(tiers = row_number()) %>%
  relocate(tiers, .before = mpg)



shinyApp(
  ui = fluidPage(
    useShinyjs(),
    helpText("Edit the table values in the 'Tiers' column to sort the table."),
    actionButton(inputId = "sort_button", label = "Sort Table"),
    actionButton(inputId = "remove_row_button", label = "Remove Row From Table", disabled = ''),
    rHandsontableOutput("cars_table")
  ),
  
  
  
  server = function(input, output, session) {
    
    
    # rv <- reactiveValues(df = cars_data)
    cars_rv <- reactiveValues(table = cars_data)
    
    output$cars_table <- renderRHandsontable({
      
      rhandsontable(data = cars_rv$table,
                    selectCallback = TRUE)
      
    })
    
    
    observe({
      if (!is.null(input$cars_table_select$select$r)) {
        shinyjs::enable("remove_row_button")
      }
    })
    
    
    observeEvent(input$remove_row_button, {
      
      selected_rhands_rows <- input$cars_table_select$select$r
      
      cars_rv$table <- cars_rv$table %>%
        slice(-c(selected_rhands_rows)) %>%
        mutate(tiers = row_number())
      
      
      
      output$cars_table <- renderRHandsontable({
        rhandsontable(data = cars_rv$table,
                      selectCallback = TRUE)
        
      })
      
    
      shinyjs::disable("remove_row_button")
      
    })
    
    
    observeEvent(input$sort_button, {
      
      edited_data <- hot_to_r(input$cars_table)
      
      edited_data <- edited_data[order(edited_data$tiers), ]
      
      cars_rv$table <- edited_data
      
    })
  }
)
