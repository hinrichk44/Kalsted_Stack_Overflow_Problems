library(shiny)
library(rhandsontable)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Iris Dataset in rhandsontable"),
  sidebarLayout(
    sidebarPanel(
      h4("Interactive Table"),
      p("Modify the table below.")
    ),
    mainPanel(
      rHandsontableOutput("iris_table"),
      uiOutput("species_warning")
    )
  )
)

# Define server logic for the app
server <- function(input, output, session) {
  output$iris_table <- renderRHandsontable({
    rhandsontable(iris)
  })
  
  observe({
    if (!is.null(input$iris_table)) {
      df <- hot_to_r(input$iris_table)
      missing_species <- which(is.na(df$Species) | df$Species == "")
      if (length(missing_species) > 0) {
        warning_message <- tags$div(
          style = "color: red;",
          "Warning: The 'Species' column contains blank or NA values. ",
          "Sepal.Length values for these rows are: ",
          tags$ul(
            lapply(missing_species, function(i) {
              tags$li(paste("Row", i, ": Sepal.Length =", df$Sepal.Length[i]))
            })
          )
        )
        output$species_warning <- renderUI({ warning_message })
      } else {
        output$species_warning <- renderUI({ NULL })
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
