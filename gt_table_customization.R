# Load the tidyverse package
library(tidyverse)


library(gt)

# Create a tibble with two columns: Project_Name and Project_Cost
my_tibble <- tibble(
  Project_Name = paste("Project", 1:10),  # Generating project names
  Project_Cost = c(5, 7, -2, -8, 10, -3, 0, 6, -5, 4)  # Specifying the project costs
)

# Print the tibble
my_tibble



# Convert the tibble to a gt table
my_table <- my_tibble %>%
  gt()

# Print the gt table
my_table




# Convert the tibble to a gt table
my_table_2 <- my_tibble %>%
  gt() %>%
  data_color(
    columns = Project_Cost,
    colors = scales::col_numeric(
      palette = c("red", "green"),
      domain = c(min(my_tibble$Project_Cost), max(my_tibble$Project_Cost))
    )
  )

# Print the formatted gt table
my_table_2




# Convert the tibble to a gt table and format the Project_Cost column
my_table_3 <- my_tibble %>%
  gt() %>%
  data_color(
    columns = Project_Cost,
    colors = scales::col_factor(
      palette = c("red", "green"),
      domain = c(0, 1)
    )
  )

# Print the formatted gt table
my_table_3



# Convert the tibble to a gt table and format the Project_Cost column
my_table_4 <- my_tibble %>%
  gt() %>%
  data_color(
    columns = Project_Cost,
    colors = function(x) {
      ifelse(x == 0, "red", "green")
    }
  ) %>%
  tab_options(column_labels.border.lr.style = "solid",
              column_labels.border.lr.width = 5,
              column_labels.border.lr.color = "blue",
              column_labels.vlines.style = "solid",
              column_labels.vlines.width = 5,
              column_labels.vlines.color = "red")

# Print the formatted gt table
my_table_4



# Create a tibble with Cost_Type, FY2024, and FY2025 columns
my_data <- tibble(
  Cost_Type = c("Programmed", "Funded", "Percent Funded"),
  FY2024 = c(runif(1, 1, 100), runif(1, 1, 100), NA), # Random values for Programmed and Funded
  FY2025 = c(runif(1, 1, 100), runif(1, 1, 100), NA)  # Random values for Programmed and Funded
)

# Calculate Percent Funded
my_data$FY2024[3] <- my_data$FY2024[2] / my_data$FY2024[1]
my_data$FY2025[3] <- my_data$FY2025[2] / my_data$FY2025[1]

# Print the tibble
print(my_data)


# Create a formatted table using gt
my_table <- my_data %>%
  gt() %>%
  tab_header(title = "Cost Type and Funding Information") %>%
  cols_label(
    Cost_Type = "Cost Type",
    FY2024 = "FY 2024",
    FY2025 = "FY 2025"
  ) %>%
  fmt_number(columns = c("FY2024", "FY2025"), decimals = 2) %>%
  data_color(
    columns = c("FY2024", "FY2025"),
    colors = scales::col_numeric(
      palette = c("#E5E5E5", "#92C5DE", "#0571B0"),
      domain = NULL
    )
  )

# Print the formatted table
my_table




# Create a formatted table using gt
my_table_2 <- my_data %>%
  gt() %>%
  tab_header(title = "Cost Type and Funding Information") %>%
  cols_label(
    Cost_Type = "Cost Type",
    FY2024 = "FY 2024",
    FY2025 = "FY 2025"
  ) %>%
  fmt_number(columns = c("FY2024", "FY2025"), decimals = 2) %>%
  data_color(
    columns = c("FY2024", "FY2025"),
    colors = scales::col_numeric(
      palette = c("#E5E5E5", "#92C5DE", "#0571B0"),
      domain = NULL
    )
  )   %>%
  tab_style(style = cell_fill(color = "blue"),
            locations = cells_body(columns = contains("FY"), 
                                   rows = (Cost_Type == "Percent Funded")))

# Print the formatted table
my_table_2



# Create a formatted table using gt
my_table_3 <- my_data %>%
  gt() %>%
  tab_header(title = "Cost Type and Funding Information") %>%
  cols_label(
    Cost_Type = "Cost Type",
    FY2024 = "FY 2024",
    FY2025 = "FY 2025"
  ) %>%
  fmt_number(columns = c("FY2024", "FY2025"), decimals = 2) %>%
  data_color(
    columns = c("FY2024", "FY2025"),
    colors = scales::col_numeric(
      palette = c("#E5E5E5", "#92C5DE", "#0571B0"),
      domain = NULL
    )
  ) %>%
  tab_style(
    style = cell_text(
      color = ifelse(my_data$Cost_Type == "Percent Funded", 
                     ifelse(my_data$FY2025 == 1, "blue", "red"),
                     "black")
    ),
    locations = cells_body(columns = c("FY2024", "FY2025"), 
                           rows = my_data$Cost_Type == "Percent Funded")
  )

# Print the formatted table
my_table_3

