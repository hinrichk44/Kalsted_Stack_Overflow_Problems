library(tidyverse)

# Create the dataframe
df <- data.frame(
  Project_Name = c(rep("Project A", 5), rep("Project B", 5)),
  Project_Status = c("Completed", "Ongoing", "Ongoing", "Completed", "Planned",
                     "Planned", "Ongoing", "Completed", "Ongoing", "Completed"),
  Project_Type = c("Research", "Development", "Development", "Research", "Development",
                   "Development", "Research", "Development", "Research", "Development"),
  Project_Cost = c(1000, 5000, 8000, 12000, 15000,
                   20000, 4000, 10000, 6000, 9000)
)

# Print the dataframe
print(df)



# Filter the dataframe to remove rows where Project Name is "Project A" and Project Status is "Completed"
filtered_df <- df[!(df$Project_Name == "Project A" & df$Project_Status == "Completed"),]

# Print the filtered dataframe
print(filtered_df)