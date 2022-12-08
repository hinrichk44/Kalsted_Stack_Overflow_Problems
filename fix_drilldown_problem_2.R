library(tidyverse)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)





full_data <- tibble(
  State = c("IL", "IL", "IL", "IL", "IL", "IL", "IN", "IN", "IN", "IN", "IN", "IN"),
  City = c("Chicago", "Rockford", "Naperville", "Chicago", "Rockford", "Naperville","Fort Wayne", "Indianapolis", "Bloomington", "Fort Wayne", "Indianapolis", "Bloomington"),
  Year = c("2008", "2008", "2008", "2009", "2009", "2009", "2008", "2008", "2008", "2009", "2009", "2009"),
  GDP = c(200, 300, 350, 400, 450, 250, 600, 400, 300, 800, 520, 375)
)





ui <- fluidPage(
  # selectInput(inputId = "year",
  #             label = "Year",
  #             multiple = TRUE,
  #             choices = unique(full_data$Year),
  #             selected = unique(full_data$Year)),
  # selectInput(inputId = "state",
  #             label = "State",
  #             choices = unique(full_data$State)),
  # selectInput(inputId = "city",
  #             label = "City",
  #             choices = unique(full_data$City)),
  # plotlyOutput("state_level", height = 200),
  # plotlyOutput("city_level", height = 200),
  
  uiOutput('back'),                          # dropdown menus
  # uiOutput("back1")         
  
  plotlyOutput('plt', height = 200)         # the plot 
)



server <- function(input, output, session) {
  
  # create drill down defaults
  dd <- reactiveValues(
    Year = NULL,
    State = NULL,
    City = NULL
  )
  
  observeEvent(event_data('plotly_click', source = 'plt'), {
    x <- event_data('plotly_click', source = "plt")$x
    if(!length(x)) return()
    if(!length(dd$Year)) {         # if year hasn't been chosen
      dd$Year <- x                 # show all years
    } else if(!length(dd$State)) { # if state hasn't been chosen
      dd$State <- x                # show all states
    } else {
      dd$City <- x                 # show all cities
    }
  })
  
  gdp_reactive <- reactive({
    if(!length(dd$Year)) {                     # if year not chosen
      return(mutate(full_data, value = Year))  # end reactive statement
    }
    full_data <- filter(full_data, Year %in% dd$Year) # dd is drill down
    if(!length(dd$State)) {                    # if state not chosen
      return(mutate(full_data, value = State)) # end reactive statement
    }
    full_data <- filter(full_data, State %in% dd$State)
    mutate(full_data, value = City)
  })
  
  output$plt <- renderPlotly({            # year not chosen initially
    p <- plot_ly(gdp_reactive(), type = "bar", showlegend = F, # so it looks like only 1 trace
                 x = ~value, y = ~GDP, source = "plt")
    if(!length(dd$State)) {               # year chosen, but state not chosen
      add_bars(p, color = ~value) %>% 
        layout(barmode = "overlay")
    } else if(!length(dd$City)) {         # year and state chosen, not city
      plot_ly(gdp_reactive(), type = "bar", showlegend = F, # remove color
              x = ~value, y = ~GDP, source = "plt")
    } else {
      add_bars(p, color = ~value) %>%     # year, state, and city chosen
        filter(value %in% dd$City) %>% # this doesn't make a difference/ only 1 obs per city/state/year
        layout(barmode = "overlay")
    }
  })
  
  output$back <- renderUI({     # create the dropdown menus
    if(!length(dd$Year)) return('Click the bars to drill down') # tip to get started
    yearInput <- selectInput(
      "Year", "Year",
      choices = unique(full_data$Year), selected = dd$Year # year dropdown
    )
    if(!length(dd$State)) return(yearInput)     # stop here; show dropdown options
    subcat <- filter(full_data, Year %in% dd$Year)
    # if not all states were represented in all years
    stateInput <- selectInput(
      "State", "State",
      choices = unique(subcat$State), 
      selected = dd$State                       # state dropdown
    )
    if(!length(dd$City)) {                      # if no cities, stop here
      return(fluidRow(column(3, yearInput),     # show dropdown options
                      column(3, stateInput)))
    }
    subcat <- filter(subcat, State %in% dd$State)
    # only cities in the drill down state in the drill down year
    cities <- selectInput(                      # city dropdown
      "City", "City",
      choices = unique(subcat$City), selected = dd$City 
    )
    fluidRow(                                   # show all dropdown options
      column(3, yearInput),
      column(3, stateInput),
      column(3, cities)
    )
  })
  
  observeEvent(input$Year, {  # observe selection from year dropdown
    dd$Year <- input$Year
    dd$State <- NULL
    dd$City <- NULL
  })
  observeEvent(input$State, { # observe selection from state dropdown
    dd$State <- input$State
    dd$City <- NULL
  })
  observeEvent(input$City, {  # observe selection from city dropdown
    dd$City <- input$City
  })
}



shinyApp(ui, server)