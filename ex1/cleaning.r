# Load packages
library(tidyverse)

# Set your working directory to where the csv files are
setwd("/Users/ren/Downloads/conj_data")

# List all csv files
Raw_ex1_an_gp1  <- read_csv("Raw_ex1-an-gp1.csv")
Raw_ex1_an_gp2  <- read_csv("Raw_ex1-an-gp2.csv")
Raw_ex1_an_gp3  <- read_csv("Raw_ex1-an-gp3.csv")
Raw_ex1_an_gp4  <- read_csv("Raw_ex1-an-gp4.csv")
Raw_ex1_an_gp5  <- read_csv("Raw_ex1-an-gp5.csv")
Raw_ex1_an_gp6  <- read_csv("Raw_ex1-an-gp6.csv")

Template_ex1_and_gp1 <- read_csv("Template_ex1-and-gp1.csv")
Template_ex1_and_gp2 <- read_csv("Template_ex1-and-gp2.csv")
Template_ex1_and_gp3 <- read_csv("Template_ex1-and-gp3.csv")
Template_ex1_and_gp4 <- read_csv("Template_ex1-and-gp4.csv")
Template_ex1_and_gp5 <- read_csv("Template_ex1-and-gp5.csv")
Template_ex1_and_gp6 <- read_csv("Template_ex1-and-gp6.csv")

library(dplyr)

# Put the raw dfs into a list
raw_list <- list(
  Raw_ex1_an_gp1,
  Raw_ex1_an_gp2,
  Raw_ex1_an_gp3,
  Raw_ex1_an_gp4,
  Raw_ex1_an_gp5,
  Raw_ex1_an_gp6
)

raw_list_clean <- lapply(raw_list, function(df) {
  
  # Step 1: Drop first 22 columns
  df2 <- df %>% select(-(1:22))
  
  # Step 2: Save first row as column names
  new_names <- as.character(df2[1, ])
  
  # Step 3: Remove rows 1 and 3 (original indexing)
  df2 <- df2[-c(1,2), ]
  
  # Step 4: Assign column names
  colnames(df2) <- new_names
  
  df2
})

# Put back into environment
Raw_ex1_an_gp1 <- raw_list_clean[[1]]
Raw_ex1_an_gp2 <- raw_list_clean[[2]]
Raw_ex1_an_gp3 <- raw_list_clean[[3]]
Raw_ex1_an_gp4 <- raw_list_clean[[4]]
Raw_ex1_an_gp5 <- raw_list_clean[[5]]
Raw_ex1_an_gp6 <- raw_list_clean[[6]]

for (i in 1:6) {
  df_name <- paste0("Raw_ex1_an_gp", i)
  df <- get(df_name)
  
  df <- df %>%
    mutate(participant_id = paste0("group", i, "_", row_number())) %>%
    relocate(participant_id, .before = 1)
  
  assign(df_name, df, envir = .GlobalEnv)
}

for (i in 1:6) {
  
  df_name <- paste0("Raw_ex1_an_gp", i)
  df <- get(df_name)
  
  df <- df %>%
    mutate(
      participant_id = paste0("group_", i, "_", row_number()),
      Group = i
    ) %>%
    relocate(participant_id, Group, .before = 1)
  
  assign(df_name, df, envir = .GlobalEnv)
}


for (i in 1:6) {
  
  df_name <- paste0("Raw_ex1_an_gp", i)
  df <- get(df_name)
  
  # Number of question columns
  n_questions <- ncol(df) - 2
  
  # Rename columns
  colnames(df)[3:ncol(df)] <- paste0("Q", 1:n_questions)
  
  assign(df_name, df, envir = .GlobalEnv)
}


for (i in 1:6) {
  
  df_name <- paste0("Raw_ex1_an_gp", i)
  df <- get(df_name)
  
  df_long <- df %>%
    pivot_longer(
      cols = 3:ncol(df),      # Questions start at column 3
      names_to = "Questions",
      values_to = "Response"
    )
  
  assign(paste0(df_name, "_long"), df_long, envir = .GlobalEnv)
}

for (i in 1:6) {
  
  df_name <- paste0("Template_ex1_and_gp", i)
  df <- get(df_name)
  
  df <- df %>%
    mutate(Questions = paste0("Q", row_number())) %>%
    relocate(Questions, .before = 1)
  
  assign(df_name, df, envir = .GlobalEnv)
}

# merge Templates with Results by Quesions

for (i in 1:6) {
  
  raw_name <- paste0("Raw_ex1_an_gp", i, "_long")
  template_name <- paste0("Template_ex1_and_gp", i)
  
  raw_df <- get(raw_name)
  template_df <- get(template_name)
  
  merged_df <- raw_df %>%
    left_join(template_df, by = c("Questions" = "Questions"))
  
  assign(paste0("Merged_gp", i), merged_df, envir = .GlobalEnv)
}

# write cleaned data

for (i in 1:6) {
  
  df_name <- paste0("Merged_gp", i)
  df <- get(df_name)
  
  write.csv(
    df,
    file = paste0("Merged_gp", i, ".csv"),
    row.names = FALSE
  )
}
