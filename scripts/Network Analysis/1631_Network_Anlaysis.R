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

df_1631 <- read.csv("cleaned_plague_data_1631.csv")
names(df_1631) <- gsub("\\.", "_", names(df_1631))
head(df_1631)

#----BUILDING TABLES--------# 

## Gender vs. Cause of Death
gender_cause_table <- df_1631 %>%
  count(Sex, Type_of_Death) %>%
  pivot_wider(names_from = Type_of_Death, values_from = n, values_fill = 0)
print(gender_cause_table)

## Age vs. Cause of Death 
total_deaths_by_age <- df_1631 %>% 
  count(Age_Group, Type_of_Death) %>%
  pivot_wider(names_from = Type_of_Death, values_from = n, values_fill = 0)
print(total_deaths_by_age)

# Plague Deaths by Age
plague_by_age <- total_deaths_by_age %>%
  select(Age_Group, `Plague Death`) 
print(plague_by_age)

## Plague Deaths By Gender and Age
plague_groups <- df_1631 %>%
  filter(Type_of_Death == "Plague Death") %>%
  count(Sex, Age_Group) %>%
  pivot_wider(names_from = Age_Group, values_from = n, values_fill = 0)
print(plague_groups)

## Daily Deaths
daily_plague <- df_1631 %>%
  count(Date, name = "plague_deaths") %>%
  arrange(Date)
print(daily_plague)

#----- VISUAL ANAYLSIS-----

# Types of Deaths by Gender
death_by_gender <- df_1631 %>%
  count(Sex, Type_of_Death) %>%
  ggplot(aes(x = Sex, y = n , fill = Type_of_Death)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Plague vs. Non-Plauge Deaths in 1631 by Gender",
    x = "Gender" ,
    y = "Number of Deaths",
    fill = "Cause"
  ) +
  theme_minimal()
print(death_by_gender)

# Male vs. Female Plague Deaths
plague_death_by_gender <- sex_cause_table %>%
  ggplot(aes(x = Sex, y = `Plague Death`, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "1631 Plague-Deaths by Gender",
    x = "Gender" ,
    y = "Number of Plague-Deaths",
    fill = "Cause"
  ) +
  theme_minimal()
print(plague_death_by_gender)

# Plague Deaths vs. Age 
plague_deaths_by_age <- plague_by_age %>%
  ggplot(aes(x = Age_Group, y = `Plague Death`, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "1631 Plague-Deaths by Age",
    x = "Age" ,
    y = "Number of Plague-Deaths",
    fill = "Cause"
  ) +
  theme_minimal()
print(plague_deaths_by_age)


# Plague Deaths by Age vs.Gender
plague_by_age_and_gender <- plague_groups %>%
  pivot_longer(
    cols = -Sex,
    names_to = "age_group",
    values_to = "n"
  )

ggplot(plague_by_age_and_gender, aes(x = age_group, y = n, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Plague Deaths by Gender and Age",
    x = "Age Group",
    y = "Number of Plague Deaths",
    fill = "Sex"
  ) +
  theme_minimal()


#-----NETWORK MODEL--------
# Nodes = people
# Edge = two people died within k days of each other
# Restricting to plague deaths makes the network more focused

# Filtered df
network_df_1631 <- df_1631 %>%
  filter(!is.na(Date), Sex != "Unknown", Plague_Flag == 1, Afflicted_Time__Days != "Unkown") %>%
  mutate(person_id = row_number())
head(network_df_1631)
network_df_1631 <- network_df_1631 %>%
  mutate(Date = as.Date(Date))

#Time Window: 8 days 
k <- 8

# Creating Edges 
edges <- network_df_1631 %>%
  select(person_id, Date) %>%
  cross_join(
    network_df_1631 %>% select(person_id, Date),
    suffix = c("_from", "_to")
  ) %>%
  filter(person_id_from < person_id_to) %>%
  mutate(day_diff = abs(as.integer(Date_from - Date_to))) %>%
  filter(day_diff <= k)

# Calculate Edge Weight
g <- graph_from_data_frame(edges, directed = FALSE)
avg_degree <- mean(degree(g))
N_t = 440 + 419
avg_degree
# New Beta Term
alpha <- 2
beta_net <- beta_0 * (1 + alpha * avg_degree / N_t)
beta_net

#----------NEW SIR MODEL-----------------##
# Modified SIR Model
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R                  
    dS <- -beta_net * S * I / N
    dI <-  beta_net * S * I / N - gamma * I
    dR <-  gamma * I
    
    list(c(dS, dI, dR))
  })
}

# Assign Initial Values and Parameters##
parameters_values <- c(
  beta_net <- beta_net,      # This is where Beta will differ from original model     
  gamma <- 0.125
)
initial_values <- c(
  S = N_t - 1,        # Aussumed Population at Day 1 (using total deaths as population) in Venice At 1/03/1630
  I = 1,              # Number of People infected on 01/03/1630
  R = 0               # Number of Recovered People
)

time_values <- seq(0, 350, by = 10)     # Time span of Data in Days (1 year) Increments of 10 

## SIR Modeling 
sir_values <- ode(
  y = initial_values,
  times = time_values,
  func = sir_model,
  parms = parameters_values
)
sir_values
sir_values <- as.data.frame(sir_values)

## Plotting SIR Model 
matplot(sir_values$time, sir_values[, c("S", "I", "R")],
        type = "l", lty = 1, col = c("blue", "red", "green"),
        xlab = "Time (Days)", ylab = "Population in Venice")
legend("right", c("Susceptibles", "Infectious", "Recovered/Dead"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")

## Isolating the Infectious Pupulation 
plot(sir_values$time, sir_values$I, type = "l", col = "red",
     xlab = "Time (Days)", ylab = "Infectious Population")

## R0 Value Calculation
(100000 + 1) * beta_net / 0.125

