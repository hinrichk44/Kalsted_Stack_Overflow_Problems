library(tidyverse)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)


store_data <- tibble(
  Whole_Foods = c(1000, 500, 500, 1000, 500, 500),
  Kroger = c(700, 300, 400, 700, 300, 400),
  Jewel = c(800, 0, 0, 800, 0, 0),
  Food_Main = c("Vegetable", "Lettuce", "Potato", "Fruit", "Lemon", "Watermelon"),
  Food_Filter = c("None", "Vegetable", "Vegetable", "None", "Fruit", "Fruit")
  
)


store_data <- store_data %>%
  reshape2::melt(measure.vars = c("Whole_Foods", "Kroger", "Jewel"),
                 variable.name = "Grocery_Store") %>%
  mutate(value = value %>% as.numeric()) %>%
  rename(Sales = value)


ui <- fluidPage(
  selectInput(inputId = "store",
              label = "Grocery Store",
              multiple = TRUE,
              choices = unique(store_data$Grocery_Store),
              selected = unique(store_data$Grocery_Store)),
  selectInput(inputId = "food_subcategory",
              label = "Food Type",
              choices = c("Vegetable", "Fruit")),
  plotlyOutput("food_level", height = 200),
  plotlyOutput("filter_level", height = 200),
  uiOutput('back'),
  uiOutput("back1")
)


server <- function(input, output, session) {
  
  
  food_filter <- reactiveVal()
  type_filter <- reactiveVal()
  
  
  observeEvent(event_data("plotly_click", source = "food_level"), {
    food_filter(event_data("plotly_click", source = "food_level")$x)
    type_filter(NULL)
  })
  
  
  observeEvent(event_data("plotly_click", source = "filter_level"), {
    type_filter(
      event_data("plotly_click", source = "filter_level")$x
    )
  })
  
  
  store_reactive <- reactive({
    store_data %>%
      filter(Food_Filter ==  "None") %>%
      filter(Grocery_Store %in% input$store)
  })
  
  
  output$food_level <- renderPlotly({
    store_reactive() %>% 
      plot_ly(
        x = ~Grocery_Store,
        y = ~Sales,
        color = ~Food_Main,
        source = "food_level",
        type = "bar"
      ) %>% 
      layout(barmode = "stack", showlegend = T)
  })
  
  
  store_reactive_2 <- reactive({
    store_data %>%
      filter(Grocery_Store %in% input$store) %>%
      filter(Food_Filter %in% input$food_subcategory)
  })
  
  
  output$filter_level <- renderPlotly({
    if (is.null(food_filter())) return(NULL)
    
    store_reactive_2() %>% 
      plot_ly(
        x = ~Grocery_Store,
        y = ~Sales,
        color = ~Food_Main,
        source = "food_level",
        type = "bar"
      ) %>% 
      layout(barmode = "stack", showlegend = T)
  })
  
  
  output$back <- renderUI({
    if (!is.null(food_filter()) && is.null(type_filter())) {
      actionButton("clear", "Back", icon("chevron-left"))
    }
  })
  
  
  output$back1 <- renderUI({
    if (!is.null(type_filter())) {
      actionButton("clear1", "Back", icon("chevron-left"))
    }
  })
  
  
  observeEvent(input$clear,
               food_filter(NULL))
  observeEvent(input$clear1,
               type_filter(NULL))
  
}


shinyApp(ui, server)
