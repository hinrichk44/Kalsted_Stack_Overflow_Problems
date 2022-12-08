library(tidyverse)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)





full_data <- tibble(
  State = c("IL", "IL", "IL", "IL", "IL", "IL", "IN", "IN", "IN", "IN", "IN", "IN"),
  City = c("Chicago", "Rockford", "Naperville", "Chicago", "Rockford", "Naperville","Fort Wayne", "Indianapolis", "Bloomington", "Fort Wayne", "Indianapolis", "Bloomington"),
  Year = c("2008", "2008", "2008", "2009", "2009", "2009", "2008", "2008", "2008", "2009", "2009", "2009"),
  GDP = c(200, 300, 350, 400, 450, 250, 600, 400, 300, 800, 520, 375),
  Rating = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A")
)






ui <- fluidPage(
  selectInput(inputId = "year",
              label = "Year",
              multiple = TRUE,
              choices = unique(full_data$Year),
              selected = unique(full_data$Year)),
  selectInput(inputId = "state",
              label = "State",
              choices = unique(full_data$State)),
  selectInput(inputId = "city",
              label = "City",
              choices = unique(full_data$City)),
  verbatimTextOutput("description", placeholder = TRUE),
  plotlyOutput("state_level", height = 200),
  plotlyOutput("city_level", height = 200),
  uiOutput('back'),
  uiOutput("back1")
)






server <- function(input, output, session) {
  
  
  state_filter <- reactiveVal()
  city_filter <- reactiveVal()
  
  
  

  observeEvent(event_data("plotly_click", source = "state_level"), {
    state_filter(event_data("plotly_click", source = "state_level")$x)
    city_filter(NULL)
  })
  
  observeEvent(event_data("plotly_click", source = "city_level"), {
    city_filter(
      event_data("plotly_click", source = "city_level")$x
    )
  })
  
  

  
  gdp_reactive <- reactive({
    full_data %>%
      filter(Year %in% input$year) %>%
      filter(State %in% input$state)  
  })
  
  
  
  output$state_level <- renderPlotly({
    gdp_reactive() %>% 
      plot_ly(
        x = ~Year,
        y = ~GDP,
        color = ~City,
        source = "state_level",
        type = "bar"
      ) %>% 
      layout(barmode = "stack", showlegend = T)
    
    
  })
  
  
  
  gdp_reactive_2 <- reactive({
    full_data %>%
      filter(Year %in% input$year) %>%
      filter(State %in% input$state) %>%
      filter(City %in% input$city) 
  })
  
  
  
  output$city_level <- renderPlotly({
    if (is.null(state_filter())) return(NULL)
    
    gdp_reactive_2() %>% 
      plot_ly(
        x = ~Year,
        y = ~GDP,
        color = ~City,
        source = "city_level",
        type = "bar"
      ) %>% 
      layout(barmode = "stack", showlegend = T)
    
    
  })
  
  
  
  
  output$back <- renderUI({
    if (!is.null(state_filter()) && is.null(city_filter())) {
      actionButton("clear", "Back", icon("chevron-left"))
    }
  })
  
  output$back1 <- renderUI({
    if (!is.null(city_filter())) {
      actionButton("clear1", "Back", icon("chevron-left"))
    }
  })
  
  observeEvent(input$clear,
               state_filter(NULL))
  observeEvent(input$clear1,
               city_filter(NULL))
  
  
  
  
  
  new_rating <- reactive({
    full_data %>%
      filter(Year %in% input$year) %>%
      filter(State %in% input$state) %>%
      filter(City %in% input$city)
        })
  
  output$description <- renderText({
    
    
    if (is.na(input$state))
      {
      paste("UNKNOWN")
    }
    
    else {
      paste(c(input$state, input$city, input$year))
    }
    
  })
  
  
  
}

shinyApp(ui, server)
