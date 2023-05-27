library(tidyverse)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)


full_data <- tibble(
  Project_Level = c(0,1,1,2,2,2,2, 0,1,1,2,2,2,2),
  Project_Sublevel = c(1,2,2,3,3,3,3, 1,2,2,3,3,3,3),
  Project_Type = c("House", "Bedrooms", "Bathrooms", "Bed", "Closet", "Toliet", "Shower",
                   "House", "Bedrooms", "Bathrooms", "Bed", "Closet", "Toliet", "Shower"),
  Project_Scope = c("None", "House", "House", "Bedrooms", "Bedrooms", "Bathrooms", "Bathrooms",
                    "None", "House", "House", "Bedrooms", "Bedrooms", "Bathrooms", "Bathrooms"),
  Year = c("2008", "2008", "2008", "2008", "2008", "2008", "2008",
           "2009", "2009", "2009", "2009", "2009", "2009", "2009"),
  Cost = c(1000, 500, 500, 250, 250, 250, 250, 
           2000, 1000, 1000, 500, 500, 500, 500)
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
    inputId = "project_level",
    label = "Project Level",
    multiple = FALSE,
    choices = unique(full_data$Project_Level),
    selected = "0"
  ),
  # selectInput(
  #   inputId = "project_sublevel",
  #   label = "Project Sub-Level",
  #   multiple = FALSE,
  #   choices = unique(full_data$Project_Sublevel)
  # ),
  tags$h3("Project Sub-Level"),
  tableOutput('project_sublevel'),
  plotlyOutput("housing_cost", height = 400),
  shinyjs::hidden(actionButton("clear", "Return to Project Level"))
)


server <- function(input, output, session) {
  
  
  observeEvent({
    input$project_level
  },
  handlerExpr = {
    if (input$project_level == "<select>") {
      choice <- ""
    } else {
      choice <- as.numeric(input$project_level) + 1
    }
    updateSelectInput(
      session = session,
      inputId = "project_sublevel",
      choices = choice
    )
  })
  
  output$project_sublevelt<- renderTable({
    unique(house_reactive()$Project_Sublevel)
  })
  
  
  
  drills <- reactiveValues(category = NULL,
                           sub_category = NULL)
  
  
  house_reactive <- reactive({
    full_data %>%
      filter(Year %in% input$year) %>%
      filter(Project_Level %in% input$project_level)
  })
  
  
  house_reactive_2 <- reactive({
    full_data %>%
      filter(Year %in% input$year) %>%
      filter(Project_Level %in% input$project_sublevel) %>%
      filter(Project_Scope %in% drills$category)
  })
  
  
  house_data <- reactive({
    if (is.null(drills$category)) {
      return(house_reactive())
    }
    else {
      return(house_reactive_2())
    }
  })
  
  
  output$housing_cost <- renderPlotly({
    if (is.null(drills$category)) {
      plot_title <- paste0("Cost of Project Level Components")
    } else {
      plot_title <- paste0("Cost of ",  drills$category)
    }
    
    
    house_data() %>%
      plot_ly(
        x = ~ Year,
        y = ~ Cost,
        color = ~ Project_Type,
        key = ~ Project_Type,
        source = "housing_cost",
        type = "bar"
      ) %>%
      layout(
        barmode = "stack",
        showlegend = T,
        xaxis = list(title = "Year"),
        yaxis = list(title = "Cost"),
        title = plot_title
      )
  })
  
  
  observeEvent(event_data("plotly_click", source = "housing_cost"), {
    x <- event_data("plotly_click", source = "housing_cost")$key
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
  
  
  observeEvent(c(input$clear, input$project_level), {
    drills$category <- NULL
    shinyjs::hide("clear")
  })
}


shinyApp(ui, server)