install.packages("dplyr")
install.packages("igraph")
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(igraph)
library(ggplot2)
library(deSolve)

df = read.csv('Raw Data For 1630-31 Venice, Italy Plague Breakout (St. Eufima Perish) - Raw Data For 1631 Venice Plague.csv')
head(df)

#--------CLEANING DATA------------
##Switching names from "." to "_"
names(df) <- gsub("\\.", "_", trimws(names(df)))

# Fix Date Format 
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")

# Add Time Index
df <- df %>%
  mutate(
    days_since_start = as.integer(Date - min(Date, na.rm = TRUE))
  )

df <- df %>%
  mutate(
    plague_flag = case_when(
      tolower(Type_of_Death) == "non-plague death" ~ 0,
      tolower(Type_of_Death) == "plague death" ~ 1,
      TRUE ~ NA_real_
    )
  )

# Age Group
df <- df %>%
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
write_csv(df, "venice_plague_cleaned.csv")
df <- read.csv("venice_plague_cleaned.csv")