library(tidyverse)



df <- data.frame(ProjectName = c("Project A","Project A", "Project A","Project B", "Project B","Project B"),
                 Fiscal_Year = c("Year_1", "Year_1", "Year_2", "Year_2", "Year_2", "Year_3"),
                 Stage = c("Stage_1", "Stage_1", "Stage_1", "Stage_2", "Stage_2", "Stage_2"))
                 



filtered_df <- subset(df, !(Stage == "Stage_1" & Fiscal_Year == "Year_2"))




df_2 <- data.frame(ProjectName = c("Project A","Project A", "Project A","Project B", "Project B","Project B"),
                 Fiscal_Year = c("Year_1", "Year_2", "Year_3", "Year_2", "Year_2", "Year_3"),
                 Stage = c("Stage_1", "Stage_1", "Stage_1", "Stage_2", "Stage_2", "Stage_2"))



filtered_df_2 <- subset(df_2, !(Stage == "Stage_1" & (Fiscal_Year == "Year_2" | Fiscal_Year == "Year_3")))