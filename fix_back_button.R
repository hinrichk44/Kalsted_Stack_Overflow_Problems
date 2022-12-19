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


ui <- fluidPage(useShinyjs(),
  selectInput(inputId = "year",
              label = "Year",
              multiple = TRUE,
              choices = unique(full_data$Year),
              selected = unique(full_data$Year)),
  selectInput(inputId = "state",
              label = "State",
              choices = unique(full_data$State)),
  plotlyOutput("gdp_level", height = 200),
  shinyjs::hidden(actionButton("clear", "Return to State"))
)


server <- function(input, output, session) {
  
  
  drills <- reactiveValues(
    category = NULL,
    sub_category = NULL
  )
  
  
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
    
    if (!length(drills$category)) {
      
      return(gdp_reactive())
      
    }
   
    else {
      
      return(gdp_reactive_2())
      
    }
    
  })
  
  
  output$gdp_level <- renderPlotly({
    gdp_data() %>% 
      plot_ly(
        x = ~Year,
        y = ~GDP,
        color = ~City,
        key = ~City,
        source = "gdp_level",
        type = "bar"
      ) %>% 
      layout(barmode = "stack", showlegend = T)
    
  })
  
  

  observeEvent(event_data("plotly_click", source = "gdp_level"), {
    
    x <- event_data("plotly_click", source = "gdp_level")$key
    
    if (!length(x))
      
      return()
    
    if (!length(drills$category)) {
      
      drills$category <- x
      
    }  else {
      
      drills$sub_category <- NULL
      
    }
    
  })
  
  

#   observe({
#     
#     if(length(drills$category))
#     
#     shinyjs::toggle("clear")  
#     
#   })
#   
#   
#   observeEvent(input$clear, drills$category <- NULL)
#   
  
  
  observe({
    
    if(length(drills$category))  shinyjs::show("clear")  
    
  })
  
  observeEvent(input$clear, {
    drills$category <- NULL
    shinyjs::hide("clear")
  })
  
  
  
  
  
}


shinyApp(ui, server)