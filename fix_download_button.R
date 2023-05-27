library(tidyverse)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)


full_data <- tibble(
  State = c("IL", "IL", "IL", "IL", "IL", "IL", "IN", "IN", "IN", "IN", "IN", "IN"),
  City = c("Chicago", "Rockford", "Naperville", "Chicago", "Rockford", "Naperville","Fort Wayne", 
           "Indianapolis", "Bloomington", "Fort Wayne", "Indianapolis", "Bloomington"),
  Year = c("2008", "2008", "2008", "2009", "2009", "2009", "2008", "2008", "2008", "2009", "2009", "2009"),
  GDP = c(200, 300, 350, 400, 450, 250, 600, 400, 300, 800, 520, 375)
)


ui <- fluidPage(
  useShinyjs(),
  selectInput(
    inputId = "year",
    label = "Year",
    multiple = TRUE,
    choices = unique(full_data$Year),
    selected = unique(full_data$Year)
  ),
  selectInput(
    inputId = "state",
    label = "State",
    choices = unique(full_data$State)
  ),
  selectInput(
    inputId = "dataset",
    label = "Select Data to Download",
    choices = c("Full Data",
                "Illinois Trends")
  ),
  downloadButton(outputId = "downloadData",
                 label = "Download"),
  plotlyOutput("gdp_level", height = 400),
  shinyjs::hidden(actionButton("clear", "Return to State"))
)

server <- function(input, output, session) {
  
  store_sample_PDF <- function(file_name){
    pdf(file = file_name); plot(1); dev.off()
  }
  
  
  store_sample_CSV <- function(file_name) write.csv(full_data, file = file_name)
  
  
  
  
  output$downloadData <- downloadHandler(
    contentType = 'application/zip',
    filename = function() paste0("data-", Sys.Date(), ".zip"),
    content = function(file) {
      ## set name for directory to be zipped:
      bundle_dir_name <- file.path(tempdir(),'bundle')
      ## create fresh zipping directory:
      if(dir.exists(bundle_dir_name)){
        unlink(bundle_dir_name, recursive = TRUE, force = TRUE)
      }
      dir.create(bundle_dir_name)
      
      ## replace the following with the real code:
      store_sample_PDF(file.path(bundle_dir_name, 'my_pdf.pdf'))
      store_sample_CSV(file.path(bundle_dir_name, 'my_csv.csv'))
      
      zip::zip(zipfile = file,
               files = dir(bundle_dir_name, full.names = TRUE),
               mode = 'cherry-pick'
      )
    }
  )
  
  
  drills <- reactiveValues(category = NULL,
                           sub_category = NULL)
  
  
  gdp_reactive <- reactive({
    full_data %>%
      filter(Year %in% input$year) %>%
      filter(State %in% input$state)
  })
  
  
  gdp_reactive_2 <- reactive({
    full_data %>%
      filter(Year %in% input$year) %>%
      filter(State %in% input$state) %>%
      filter(City %in% drills$category)
  })
  
  
  gdp_data <- reactive({
    if (is.null(drills$category)) {
      return(gdp_reactive())
    }
    else {
      return(gdp_reactive_2())
    }
  })
  
  
  output$gdp_level <- renderPlotly({
    if (is.null(drills$category)) {
      plot_title <- paste0("GDP Level of ",  input$state)
    } else {
      plot_title <- paste0("GDP Level of ",  drills$category)
    }
    
    
    gdp_data() %>%
      plot_ly(
        x = ~ Year,
        y = ~ GDP,
        color = ~ City,
        key = ~ City,
        source = "gdp_level",
        type = "bar"
      ) %>%
      layout(
        barmode = "stack",
        showlegend = T,
        xaxis = list(title = "Year"),
        yaxis = list(title = "GDP"),
        title = plot_title
      )
  })
  
  
  observeEvent(event_data("plotly_click", source = "gdp_level"), {
    x <- event_data("plotly_click", source = "gdp_level")$key
    if (is.null(x))
      return(NULL)
    if (is.null(drills$category)) {
      drills$category <- unlist(x)
    }  else {
      drills$sub_category <- NULL
    }
  })
  
  
  observe({
    if (!is.null(drills$category)) {
      shinyjs::show("clear")
    }
  })
  
  
  observeEvent(c(input$clear, input$state), {
    drills$category <- NULL
    shinyjs::hide("clear")
  })
}


shinyApp(ui, server)