library(tidyverse)


# create empty dataframe
df <- data.frame(ProjectName = c("Project A","Project A","Project A","Project A","Project A"),
                 Year = c("2000","2000","2000","2000","2000"),
                 Forecasted = c("Actual", "Predicted","Predicted","Predicted","Predicted"),
                 StartDate = c("2000", "2001", "2002", "2003", "2004"),
                 EndDate = c("2001", "2002", "2003", "2004", "2005"),
                 Stage = c("Planning", "Gathering", "Starting", "Editing", "Ending"))




test_gantt <- df %>%  
  ggplot(aes(x = `StartDate`, xend =`EndDate`,
             y = `Year`, yend = `Year`,
             color = `Stage`)) +  
  geom_segment(size=8) +
  labs( 
    x = 'Start and Finish Dates', 
    y = 'Year At Time of Forecast')


test_gantt


df %>%  
  ggplot(aes(x = `StartDate`, xend =`EndDate`,
             y = `Year`, yend = `Year`,
             color = `Stage`, alpha = Forecasted)) +  
  geom_segment(size = 8) +
  labs( 
    x = 'Start and Finish Dates', 
    y = 'Year At Time of Forecast') +
  scale_alpha_manual(values = c(1, 0.25))
