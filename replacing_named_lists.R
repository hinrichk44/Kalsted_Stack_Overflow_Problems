library(tidyverse)


description_tibble <- tibble(
  Project_Description = c("Commerical", "Residential")
)


# Create a tibble with two columns: Project_Name and Project_Cost
project_tibble_1 <- tibble(
  Project_Name = paste("Project", 1:10),  # Generating project names
  Project_Cost = c(5, 7, -2, -8, 10, -3, 0, 6, -5, 4)  # Specifying the project costs
)


project_tibble_2 <- tibble(
  Project_Name = paste("Project", 11:20),  # Generating project names
  Project_Cost = c(5, 7, -2, -8, 10, -3, 0, 6, -5, 4)  # Specifying the project costs
)

my_list <- list("Project_Purpose" = description_tibble, "Project_Summary" =  project_tibble_1)



my_list["Project_Summary"] <- list(project_tibble_2)







# Replace "Project Summary" in my_list with new_tibble
my_list[["Project Summary"]] <- list(new_tibble)




# Replace "Project Summary" in my_list with new_tibble
assign("Project Summary", new_tibble, envir = .GlobalEnv)


a <- my_list[1]
class(a)
## returns "list"

a <- my_list[[1]]
class(a)



replace(my_list["Project_Summary"], my_list["Project_Summary"] == my_tibble, new_tibble)





# Replace "Project Summary" in my_list with new_tibble
# Replace "Project Summary" in my_list with new_tibble
my_list[["Project Summary"]] <- NULL
my_list[["Project Summary"]] <- new_tibble



new_list <- replace(my_list, my_list$Project_Summary == my_tibble, new_tibble)


# # Replace "Project Summary" in my_list with new_tibble
# my_list[["Project Summary"]] <- new_tibble



# Replace my_tibble with new_tibble in my_list using replace()
my_list_2 <- replace(my_list, "Project Summary", new_tibble)


# Replace my_tibble with new_tibble in my_list
my_list[["Project Summary"]] <- NULL
my_list[["Project Summary"]] <- new_tibble





my_list_2 <- replace(my_list, "Project Summary" =  new_tibble)


new_list <- list("Number" = 5)

new_list <- replace(new_list, new_list$Number == 5, 6)
