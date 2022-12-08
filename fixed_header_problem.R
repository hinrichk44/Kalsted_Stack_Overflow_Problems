# 1.0 Loading Libraries ----

#install.packages("reshape2")

# tidyverse contains: dplyr, ggplot2, tidyr, stringr, forcats, tibble, purrr, readr
library(tidyverse)

# shiny integrates user interface elements and reactivity
library(shiny)

# shinydashboard allows us to actually build the dashboard
library(shinydashboard)

# shinyWidgets offers custom widgets and other components to enhance your shiny applications.
library(shinyWidgets)

# tidyquant for financial analysis. Has nice ggplot2 themes
library(tidyquant)

# DT is used for making tables
library(DT)

library(reshape2)


# 2.0 Load Data ----


# Here I am importing the data I need for analysis

data <- iris


# 3.0 Cleaning Data ----



iris_summed <- data %>%
  group_by(Species) %>%
  summarize(Petal.Width.Sum = sum(Petal.Width),
            Petal.Length.Sum = sum(Petal.Length),
            Sepal.Width.Sum = sum(Sepal.Width),
            Sepal.Length.Sum = sum(Sepal.Length)) %>%
  ungroup() %>%
  reshape2::melt(measure.vars = c("Sepal.Length.Sum", "Sepal.Width.Sum", "Petal.Length.Sum", "Petal.Width.Sum"),
                 variable.name = "Characteristics") %>%
  mutate(value = value %>% as.numeric()) %>%
  rename(Numerical = value)





iris_palettes <- iris_summed %>%
  distinct(Species, Characteristics) %>%
  mutate(iris_fill = case_when(Species == "setosa" ~ "#56B4E9",
                               Species == "versicolor" ~ "#009E73",
                               Species == "virginica" ~ "#CC79A7"))

new_pal <- distinct(iris_palettes, Characteristics, iris_fill) %>% deframe()



iris_summed$x <- new_pal




# 4.0 Shiny User Interface ----

# Here I am outlining just the UI part of the Shiny dashboard 

ui <- dashboardPage(title = "Iris Data Evaulation", skin = "blue",
                    
                    dashboardHeader(title = "Iris Dashboard"),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        sidebarSearchForm("searchtext", "buttonSearch", "Search"),
                        menuItem("Iris Dataset",
                                 tabName = "iris_dataset",
                                 icon = icon("fas fa-chart-bar"))
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "iris_dataset",
                                fluidRow(box(width = 3,
                                             height = 400,
                                             selectInput(inputId = "iris_id",
                                                         label = h5(strong("Iris Information")),
                                                         choices = unique(data$Species),
                                                         selected = "")),
                                         box(width = 9,
                                             height = 400,
                                             h5(strong("Iris Breakdown")),
                                             DT::dataTableOutput("iris_table"), 
                                             style = "height:400px; overflow-y: scroll"),
                                         box(width = 12,
                                             height = 400,
                                             title = "Iris Chart",
                                             status = "primary",
                                             solidHeader = T,
                                             plotOutput("iris_chart"))
                                         
                                         
                                )))))




# 5.0 Shiny Server ----

# Here I am outlining just the server part of the Shiny dashboard 
server <- function(input, output, session) {
  
  
  
  iris_tbl <- reactive({
    data %>%
      filter(Species %in% input$iris_id)
  })
  
  
  
  output$iris_table <- DT::renderDataTable({
    
    iris_tbl()},
    rownames = FALSE,
    extensions = "FixedHeader",
    options = list(
      scrollX = TRUE,
      scrollY = "450px",
      autoWidth = TRUE,
      fixedHeader = TRUE,
      pageLength = 10,
      lengthMenu = c(10, 15),
      dom = "pt"
    ))
  
  
  
  
  iris_filter <- reactive({
    iris_summed %>%
      filter(Species %in% input$iris_id)
  })
  
  
  
  output$iris_chart <- renderPlot({
    
    plot_1 <- iris_filter()
    bbb <- unique(iris_summed$Characteristics)
    if(bbb %in% plot_1$Characteristics) {
    
    
    
      actual_plot <- iris_filter() %>%
        ggplot(aes(Species, Numerical, fill = Characteristics)) +
        geom_col(width = 0.5) +
        scale_fill_manual(values = iris_summed$x) +
        theme_tq()+
        labs(
          title = "Characteristics Per Iris Species",
          x = "Iris Species",
          y = "Iris Characteristics"
        )}
    
    
    else {
      
      actual_plot <- iris_filter() %>%
        ggplot(aes(Species, Numerical, fill = Characteristics)) +
        geom_col(width = 0.5) +
        scale_fill_tq() +
        theme_tq()+
        labs(
          title = "Characteristics Per Iris Species",
          x = "Iris Species",
          y = "Iris Characteristics"
        )
      
    }
    
    actual_plot
    
    })
  
}



# 6.0 Connecting UI with Server ----

shinyApp(ui, server)
