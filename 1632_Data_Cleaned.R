library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(igraph)
library(ggplot2)
library(deSolve)

year1_df = read.csv('/Users/nicolezainal/Downloads/Raw Data For 1630 Plague.csv')
head(year1_df)


#--------CLEANING DATA------------
##Switching names from "." to "_"
names(year1_df) <- gsub("\\.", "_", trimws(names(year1_df)))

# Fix Date Format 
year1_df$Date <- as.Date(year1_df$Date, format = "%m/%d/%y")
head(year1_df)

# Add Time Index
year1_df <- year1_df %>%
  mutate(
    days_since_start = as.integer(Date - min(Date, na.rm = TRUE))
  )

year1_df <- year1_df %>%
  mutate(
    plague_flag = case_when(
      tolower(Type_of_death) == "non-plague death" ~ 0,
      tolower(Type_of_death) == "plague death" ~ 1,
      TRUE ~ NA_real_
    )
  )

# Age Group
year1_df <- year1_df %>%
  mutate(
    age_group = case_when(
      is.na(Age) ~ "Unknown",
      Age < 1 ~ "Infant",
      Age < 13 ~ "Child",
      Age < 20 ~ "Teen",
      Age < 40 ~ "Young Adult",
      Age < 60 ~ "Middle Age",
      TRUE ~ "Older Adult"
    )
  )
## Cleaned CSV File
write_csv(year1_df, "venice_plague_1630_cleaned.csv")
year1_df <- read.csv("venice_plague_1630_cleaned.csv")