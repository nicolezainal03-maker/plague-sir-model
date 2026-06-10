library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(igraph)
library(ggplot2)
library(deSolve)

year1_df <- read.csv("venice_plague_1630_cleaned.csv")

#----BUILDING TABLES--------# 
year1_df <- year1_df %>%
  filter(Sex != 'Unkown' & Sex != "")
## Gender vs. Cause of Death
sex_cause_table_30 <- year1_df %>%
  count(Sex, Type_of_death) %>%
  pivot_wider(names_from = Type_of_death, values_from = n, values_fill = 0)
print(sex_cause_table_30)

## Age vs. Cause of Death 
total_deaths_by_age_30 <- year1_df %>% 
  count(age_group, Type_of_death) %>%
  pivot_wider(names_from = Type_of_death, values_from = n, values_fill = 0)
print(total_deaths_by_age_30)

# Plague Deaths by Age
plague_by_age_30 <- total_deaths_by_age_30 %>%
  select(age_group, `Plague Death`) 
print(plague_by_age_30)

## Plague Deaths By Gender and Age
plague_groups_30 <- year1_df %>%
  filter(Type_of_death == "Plague Death") %>%
  count(Sex, age_group) %>%
  pivot_wider(names_from = age_group, values_from = n, values_fill = 0)
print(plague_groups_30)

## Daily Deaths
daily_plague_30 <- year1_df %>%
  count(Date, name = "plague_deaths") %>%
  arrange(Date)
print(daily_plague_30)

#----- VISUAL ANAYLSIS-----

# Types of Deaths by Gender
Death_by_gender_30 <- year1_df %>%
  count(Sex, Type_of_death) %>%
  ggplot(aes(x = Sex, y = n , fill = Type_of_death)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Plague vs. Non-Plauge Deaths 1630 by Gender",
    x = "Sex" ,
    y = "Number of Deaths",
    fill = "Cause"
  ) +
  theme_minimal()
print(Death_by_gender_30)

# Male vs. Female Plague Deaths
plague_death_by_gender_30 <- sex_cause_table_30 %>%
  ggplot(aes(x = Sex, y = `Plague Death`, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "1630 Plague-Deaths by Gender",
    x = "Gender" ,
    y = "Number of Plague-Deaths",
    fill = "Cause"
  ) +
  theme_minimal()
print(plague_death_by_gender_30)

# Plague Deaths vs. Age 
plague_deaths_by_age_30 <- plague_by_age_30 %>%
  ggplot(aes(x = age_group, y = `Plague Death`, fill = age_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = " 1630 Plague-Deaths by Age",
    x = "Age" ,
    y = "Number of Plague-Deaths",
    fill = "Cause"
  ) +
  theme_minimal()
print(plague_deaths_by_age_30)


# Plague Deaths by Age vs. Gender
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
network_df_year1 <- year1_df %>%
  filter(!is.na(Date), Sex != "Unknown", plague_flag == 1, Days_Afflicted != "Unkown") %>%
  mutate(person_id = row_number())
network_df_year1 <- network_df_year1 %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
class(network_df_year1$Date)

#Time Window: 8 days 
k <- 8

# Creating Edges 
edges_year1 <- network_df_year1 %>%
  select(person_id, Date) %>%
  cross_join(
    network_df_year1 %>% select(person_id, Date),
    suffix = c("_from", "_to")
  ) %>%
  filter(person_id_from < person_id_to) %>%
  mutate(day_diff = abs(as.integer(Date_from - Date_to))) 

# Calculate Edge Weight
g_y1 <- graph_from_data_frame(edges_year1, directed = FALSE)
avg_degree <- mean(degree(g_y1))
year1_N_t = 737 + 186

# New Beta Term
alpha <- 2
beta_net <- beta_0 * (1 + alpha * avg_degree / year1_N_t)
beta_net

#----------NEW SIR MODEL-----------------##
# Modified SIR Model
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R                  
    dS <- -beta * S * I / N
    dI <-  beta * S * I / N - gamma * I
    dR <-  gamma * I
    
    list(c(dS, dI, dR))
  })
}

# Assign Initial Values and Parameters##
parameters_values <- c(
  beta <- beta_net,      # This is where Beta will differ from original model     
  gamma <- 0.125
)
initial_values <- c(
  S = N_t - 1,         # Population of Sucepetabe People in Venice At 1/03/1630
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
