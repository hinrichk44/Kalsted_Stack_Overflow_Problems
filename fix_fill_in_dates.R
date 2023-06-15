library(tidyverse)


# create empty dataframe
df <- data.frame(ProjectName = c("Project A","Project A"),
                 Stage = c("Planning", "Gathering"),
                 Season = c("Fall", "Fall"),
                 StartDate = c(as.Date("2000-01-01"), as.Date("2000-01-01")),
                 EndDate = c(as.Date("2001-01-01"), as.Date(NA)))

df <- df %>%
  arrange(Stage)




df <- df %>%
  arrange(Stage) %>%
  mutate(EndDate = if_else(is.na(EndDate), as.Date(EndDate[Stage == "Planning"]), EndDate))



