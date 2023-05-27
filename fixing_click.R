library(shiny)
library(plotly)
library(dplyr)


sales <- diamonds
sales$category = sales$cut
sales$sub_category = sales$color
sales$sales = sales$price
sales$order_date = sample(seq(as.Date('2020-01-01'), as.Date('2020-02-01'), by="day"),nrow(sales), replace = T)



ui <- fluidPage(
  plotlyOutput("category", height = 200),
  plotlyOutput("sub_category", height = 200),
  plotlyOutput("sales", height = 300),
  DT::dataTableOutput("datatable")
)

# avoid repeating this code
axis_titles <- . %>%
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = "Sales")
  )

server <- function(input, output, session) {
  
  # for maintaining the state of drill-down variables
  category <- reactiveVal()
  sub_category <- reactiveVal()
  order_date <- reactiveVal()
  
  # when clicking on a category, 
  observeEvent(event_data("plotly_click", source = "category"), {
    category(event_data("plotly_click", source = "category")$x)
    sub_category(NULL)
    order_date(NULL)
  })
  
  observeEvent(event_data("plotly_click", source = "sub_category"), {
    sub_category(
      event_data("plotly_click", source = "sub_category")$x
    )
    order_date(NULL)
  })
  
  observeEvent(event_data("plotly_click", source = "order_date"), {
    order_date(event_data("plotly_click", source = "order_date")$x)
  })
  
  output$category <- renderPlotly({
    print(category())
    if (is.null(category())) {
      plot_data <- sales %>%
        count(category, wt = sales) %>%
        mutate(current_color = "blue")
    } else {
      plot_data <- sales %>%
        count(category, wt = sales) %>%
        mutate(current_color = if_else(category %in% category(), "red", "blue"))
    }
    plot_ly(
      plot_data, x = ~category, y = ~n, source = "category", type = "bar",
      marker = list(color = ~current_color)
    ) %>%
      axis_titles() %>% 
      layout(title = "Sales by category")
  })
  
  output$sub_category <- renderPlotly({
    if (is.null(category())) return(NULL)
    sales %>%
      filter(category %in% category()) %>%
      count(sub_category, wt = sales) %>%
      mutate(current_color = if_else(sub_category %in% sub_category(), "green", "red")) %>%
      plot_ly(
        x = ~sub_category, y = ~n, source = "sub_category", type = "bar",
        marker = list(color = ~current_color)
      ) %>%
      axis_titles() %>%
      layout(title = category())
  })
  
  output$sales <- renderPlotly({
    if (is.null(sub_category())) return(NULL)
    sales %>%
      filter(sub_category %in% sub_category()) %>%
      count(order_date, wt = sales) %>%
      plot_ly(x = ~order_date, y = ~n, source = "order_date", line = list(color = "green")) %>%
      add_lines() %>%
      axis_titles() %>%
      layout(title = paste(sub_category(), "sales over time"))
  })
  
  output$datatable <-  DT::renderDataTable({
    if (is.null(order_date())) return(NULL)
    
    sales %>%
      filter(
        sub_category %in% sub_category(),
        as.Date(order_date) %in% as.Date(order_date())
      )
  })
  
}

shinyApp(ui, server)