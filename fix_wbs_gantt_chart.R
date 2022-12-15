library(tidyverse)


project_data <- tibble(
  Project_Name = c("Warehouse"),
  Proposed_Start = c("05-01-2022"),
  Proposed_Finish = c("12-01-2022"),
  Actual_Start = c("07-01-2022"),
  Actual_Finish = c("12-31-2022")
)



project_pivot <- project_data %>%
  pivot_longer(-1, names_sep = "_", names_to = c("Type", ".value"))



project_data %>%
  pivot_longer(-1, names_sep = "_", names_to = c("Type", ".value")) %>%
  ggplot(aes(xmin = Start, xmax = Finish, color = Type,
             y = Project_Name)) +
  geom_linerange(size = 8, position = position_dodge(width = 0.5)) +
  labs(title = "Comparing Project Proposed and Actual Dates",
       x = "Start and Finish Dates",
       y = "Project Name") +
  scale_colour_manual(values = c("orange", "deepskyblue4")) +
  theme_minimal(base_size = 16)









project_data %>%
      ggplot() +
        aes(x = Proposed_Start, xend = Proposed_Finish,
            y = Project_Name, yend = Project_Name,
            color = "green") +
        geom_segment(size = 8) +
        aes(x = Actual_Start, xend = Actual_Finish,
            y = Project_Name, yend = Project_Name,
            color = "red") +
        geom_segment(size = 8) +
        labs(title = "Comparing Project Proposed and Actual Dates",
             x = "Start and Finish Dates",
             y = "Project Name")
