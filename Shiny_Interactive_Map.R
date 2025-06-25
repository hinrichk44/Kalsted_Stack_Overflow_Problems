# Load necessary libraries
library(shiny)
library(leaflet)

# UI
ui <- fluidPage(
  titlePanel("Basic Interactive Map in Shiny"),
  
  leafletOutput("mymap", height = 500)
)

# Server
server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Default OpenStreetMap tiles
      setView(lng = -74.0060, lat = 40.7128, zoom = 12) %>%  # NYC coordinates
      addMarkers(lng = -74.0060, lat = 40.7128, popup = "New York City")
  })
  
}

# Run the app
shinyApp(ui, server)
