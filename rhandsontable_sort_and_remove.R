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
    br(),
    br(),
    rHandsontableOutput("cars_table")
  ),
  
  server = function(input, output, session) {
    
    
    cars_rv <- reactiveValues(
      table = cars_data,
      original_order = 1:nrow(cars_data)
    )
    
    output$cars_table <- renderRHandsontable({
      rhandsontable(data = cars_rv$table,
                    selectCallback = TRUE) %>%
        hot_col("mpg", colWidths = 75, readOnly = T) %>%
        hot_col("cyl", colWidths = 75, readOnly = T) %>%
        hot_col("disp", colWidths = 90, readOnly = T) %>%
        hot_col("hp", colWidths = 90, readOnly = T) %>%
        hot_col("drat", colWidths = 75, readOnly = T) %>%
        hot_col("wt", colWidths = 75, readOnly = T) %>%
        hot_col("qsec", colWidths = 90, readOnly = T) %>%
        hot_col("vs", colWidths = 75, readOnly = T) %>%
        hot_col("am", colWidths = 75, readOnly = T) %>%
        hot_col("gear", colWidths = 75, readOnly = T) %>%
        hot_col("carb", colWidths = 75, readOnly = T)
      
    })
    
    
    observe({
      if (!is.null(input$cars_table_select$select$r)) {
        shinyjs::enable("remove_row_button")
      }
    })
    
    
    observeEvent(input$remove_row_button, {
      
      selected_rhands_rows <- input$cars_table_select$select$r
      
      cars_rv$table <- cars_rv$table %>%
        slice(-c(selected_rhands_rows))
      
      cars_rv$table <- cars_rv$table |> 
        mutate(tiers = dense_rank(tiers))
      
      output$cars_table <- renderRHandsontable({
        rhandsontable(data = cars_rv$table,
                      selectCallback = TRUE) %>%
          hot_col("mpg", colWidths = 75, readOnly = T) %>%
          hot_col("cyl", colWidths = 75, readOnly = T) %>%
          hot_col("disp", colWidths = 90, readOnly = T) %>%
          hot_col("hp", colWidths = 90, readOnly = T) %>%
          hot_col("drat", colWidths = 75, readOnly = T) %>%
          hot_col("wt", colWidths = 75, readOnly = T) %>%
          hot_col("qsec", colWidths = 90, readOnly = T) %>%
          hot_col("vs", colWidths = 75, readOnly = T) %>%
          hot_col("am", colWidths = 75, readOnly = T) %>%
          hot_col("gear", colWidths = 75, readOnly = T) %>%
          hot_col("carb", colWidths = 75, readOnly = T)
      })
      
      shinyjs::disable("remove_row_button")
      
    })
    
    
    observeEvent(input$sort_button, {
      edited_data <- hot_to_r(input$cars_table)
      edited_data <- edited_data[order(edited_data$tiers), ]
      cars_rv$table <- edited_data
      cars_rv$original_order <- 1:nrow(cars_rv$table)
    })
  }
)
