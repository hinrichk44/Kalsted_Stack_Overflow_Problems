
library(scales)
library(tidyverse)

# Define dataframe
df <- data.frame(
  Project_Year = c("2000-01-01", "2000-01-01", "2002-01-01", "2002-01-01", "2004-01-01", "2004-01-01"),
  Project_Cost = c("$100,000", "$200,000", "$300,000", "$200,000", "$100,000", "$300,000"),
  Project_Stage = c("Beginning", "Beginning", "Middle", "Middle", "End", "End"),
  Project_Type = c("Construction", "Labor", "Construction", "Labor", "Construction", "Labor"))

df <- df %>%
  mutate(Project_Year = as.Date(Project_Year))

# Convert Project_Cost to numeric values
df$Project_Cost <- parse_number(df$Project_Cost)

# Calculate the midpoint between the bars
df <- df %>%
  group_by(Project_Year) %>%
  mutate(midpoint = (Project_Year + 0.5))

# Create the bar chart
ggplot(df, aes(x = Project_Year, y = Project_Cost, fill = Project_Type)) +
  geom_bar(stat = "identity") +
  geom_vline(aes(xintercept = midpoint, color = Project_Stage), linetype = "solid", size = 3) +
  labs(title = "Total Cost per Year",
       x = "Year",
       y = "Cost") +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  theme_minimal()
