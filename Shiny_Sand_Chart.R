library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(colorspace)
library(RColorBrewer)
library(tidyr)

# Sample data
data <- data.frame(
  Project = c("Project A", "Project A","Project A", "Project B","Project B", "Project B","Project A", "Project A","Project A", "Project B", "Project B", "Project B","Project C", "Project C","Project C", "Project C","Project C", "Project C"),
  APPN = c("PMC", "PMC", "PMC", "CBC", "CBC", "CBC", "PMC", "PMC", "PMC", "CBC", "CBC", "CBC", "XYZ", "XYZ", "XYZ", "XYZ", "XYZ", "XYZ"),
  Year = c(2010, 2011, 2012, 2010, 2011, 2012, 2010, 2011, 2012, 2010, 2011, 2012, 2010, 2011, 2012, 2010, 2011, 2012),
  Funding = c(50, 60, 70, 80, 90, 100, 80, 100, 110, 130, 150, 160, 40, 45, 50, 70, 80, 90),
  Type = c("Programmed", "Programmed", "Programmed", "Programmed", "Programmed", "Programmed",
           "FFR", "FFR", "FFR", "FFR", "FFR", "FFR", "Programmed", "Programmed", "Programmed", "FFR", "FFR", "FFR")
)

# UI
ui <- fluidPage(
  titlePanel("FFR vs Programmed Funding"),
  sidebarLayout(
    sidebarPanel(
      selectInput("filter_choice", "Filter by APPN or Project:",
                  choices = c(
                    paste("APPN:", unique(data$APPN)),
                    paste("Project:", unique(data$Project))
                  ),
                  selected = "APPN: PMC")
    ),
    mainPanel(
      plotOutput("fundingPlot", height = "600px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$fundingPlot <- renderPlot({
    selected <- input$filter_choice
    
    if (startsWith(selected, "APPN: ")) {
      value <- sub("APPN: ", "", selected)
      filtered_data <- data %>% filter(APPN == value)
      
      if (nrow(filtered_data) == 0) return(NULL)
      
      # Same as original logic with faceting
      ffr <- filtered_data %>% filter(Type == "FFR") %>% rename(FFR = Funding)
      programmed <- filtered_data %>% filter(Type == "Programmed") %>% rename(Programmed = Funding)
      merged <- left_join(ffr, programmed, by = c("Year", "APPN"))
      
      fill_data <- merged %>%
        mutate(lower = pmin(FFR, Programmed),
               upper = pmax(FFR, Programmed),
               fill_type = ifelse(FFR > Programmed, "above", "below"))
      
      base_colors <- brewer.pal(min(length(unique(data$APPN)), 8), "Dark2")
      color_df <- data.frame(APPN = unique(data$APPN), base_color = base_colors[1:length(unique(data$APPN))])
      color_df <- color_df %>%
        mutate(
          FFR_color = base_color,
          Programmed_color = lighten(base_color, 0.5)
        )
      
      color_map <- color_df %>%
        pivot_longer(cols = c("FFR_color", "Programmed_color"),
                     names_to = "Type", values_to = "color") %>%
        mutate(
          Type = ifelse(Type == "FFR_color", "FFR", "Programmed"),
          fill_key = paste(APPN, Type)
        )
      
      label_data <- fill_data %>%
        group_by(APPN) %>%
        summarize(
          year_mid = median(Year),
          program_y = mean(pmin(FFR, Programmed)) - 0.2 * mean(pmin(FFR, Programmed)),
          ffr_y = mean(ifelse(FFR > Programmed, (FFR + Programmed)/2, NA), na.rm = TRUE)
        )
      
      ggplot() +
        geom_ribbon(data = fill_data, aes(x = Year, ymin = 0, ymax = lower, fill = paste(APPN, "Programmed"))) +
        geom_ribbon(data = filter(fill_data, FFR > Programmed), aes(x = Year, ymin = Programmed, ymax = FFR, fill = paste(APPN, "FFR"))) +
        geom_line(data = programmed, aes(x = Year, y = Programmed), color = "blue", size = 1.2) +
        geom_text(data = label_data, aes(x = year_mid, y = program_y, label = "Programmed"), color = "black", size = 4) +
        geom_text(data = label_data, aes(x = year_mid, y = ffr_y, label = "FFR"), color = "black", size = 4) +
        facet_wrap(~APPN) +
        scale_fill_manual(
          values = setNames(color_map$color, color_map$fill_key),
          guide = FALSE
        ) +
        scale_x_continuous(breaks = scales::pretty_breaks(), labels = scales::number_format(accuracy = 1)) +
        labs(title = paste("FFR Area with Programmed Funding Line - APPN:", value),
             x = "Year",
             y = "Funding") +
        theme_minimal()
      
    } else if (startsWith(selected, "Project: ")) {
      value <- sub("Project: ", "", selected)
      filtered_data <- data %>% filter(Project == value)
      
      if (nrow(filtered_data) == 0) return(NULL)
      
      ffr <- filtered_data %>% filter(Type == "FFR") %>% rename(FFR = Funding)
      programmed <- filtered_data %>% filter(Type == "Programmed") %>% rename(Programmed = Funding)
      merged <- left_join(ffr, programmed, by = c("Year", "APPN"))
      
      fill_data <- merged %>%
        mutate(lower = pmin(FFR, Programmed),
               upper = pmax(FFR, Programmed),
               fill_type = ifelse(FFR > Programmed, "above", "below"))
      
      ggplot() +
        geom_ribbon(data = fill_data, aes(x = Year, ymin = 0, ymax = lower, fill = "Programmed")) +
        geom_ribbon(data = filter(fill_data, FFR > Programmed), aes(x = Year, ymin = Programmed, ymax = FFR, fill = "FFR")) +
        geom_line(data = programmed, aes(x = Year, y = Programmed), color = "blue", size = 1.2) +
        scale_fill_manual(values = c("FFR" = "#1b9e77", "Programmed" = lighten("#1b9e77", 0.5))) +
        scale_x_continuous(breaks = scales::pretty_breaks(), labels = scales::number_format(accuracy = 1)) +
        labs(title = paste("FFR vs Programmed Funding - Project:", value),
             x = "Year",
             y = "Funding") +
        theme_minimal()
    } else {
      return(NULL)
    }
  })
}

shinyApp(ui, server)
