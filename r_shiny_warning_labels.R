library(shiny)
library(rhandsontable)

ui <- fluidPage(
  titlePanel("Iris Dataset with Explanation"),
  mainPanel(
    rHandsontableOutput("irisTable"),
    br(),
    uiOutput("feedback")
  )
)

server <- function(input, output) {
  # Initialize iris dataset with an empty Explanation column
  iris$Explanation <- ""
  
  # Reactive expression to store and update the table
  reactiveTable <- reactiveVal(iris)
  
  output$irisTable <- renderRHandsontable({
    rhandsontable(reactiveTable())
  })
  
  # Observe changes in the table and update the reactive variable
  observeEvent(input$irisTable, {
    df <- hot_to_r(input$irisTable)
    reactiveTable(df)
  })
  
  # Generate feedback text based on the Explanation column
  output$feedback <- renderUI({
    df <- reactiveTable()
    blankExplanations <- df$Explanation == ""
    if (any(blankExplanations)) {
      blankSepalLengths <- df$Sepal.Length[blankExplanations]
      HTML(paste("<span style='color: red; font-weight: bold;'>",
                 "These Sepal.Length values have blank explanations:",
                 paste(blankSepalLengths, collapse=", "),
                 "</span>",
                 "<br><span style='color: red; font-weight: bold;'>Please fill them out.</span>"))
    } else {
      HTML("")
    }
  })
}

shinyApp(ui = ui, server = server)
