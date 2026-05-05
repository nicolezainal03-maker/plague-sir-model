#-----------LIBRARY UPLOADS---------------
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(igraph)
library(ggplot2)
library(deSolve)

# Cleaned Data Set Upload & Variable Fix 
clean_df <- read.csv("venice_plague_1630_cleaned.csv")
clean_df$Date <- as.Date(clean_df$Date, format = "%Y-%m-%d")

#--------NETWORK ANALYSIS FOR MALES-----------
#Isolate by Gender
male_df <- clean_df %>%
  filter(!is.na(Date), Sex == 'M', plague_flag == 1, Days_Afflicted != "Unkown") |>
  mutate(person_id_m = row_number())
head(male_df)

# Time Window: 8 days 
k <- 8

## Show Edges between people who were Infected in the Time Window
edges_m <- male_df %>%
  select(person_id_m, Date) %>%
  cross_join(
    male_df %>% select(person_id_m, Date),
    suffix = c("_from", "_to")
  ) %>%
  filter(person_id_m_from < person_id_m_to) %>%
  mutate(day_diff = abs(as.integer(Date_from - Date_to))) %>%
  filter(day_diff <= k)
count(edges_m)

# Calculate Edge Weight
g_m <- graph_from_data_frame(edges_m, directed = FALSE)
avg_degree_m <- mean(degree(g_m))
N_m <- sum(clean_df$Sex == "M")
avg_degree_m

# Create New Beta Term
alpha <- 2
beta_0 <- 0.1625
beta_net_m <- beta_0 * (1 + alpha * avg_degree_m / N_m)
beta_net_m

#----------NEW SIR MODEL-----------------##
# Things That wont Change
sir_model_m <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R                  
    dS <- -beta_m * S * I / N
    dI <-  beta_m * S * I / N - gamma * I
    dR <-  gamma * I
    
    list(c(dS, dI, dR))
  })
}
alpha <- 2
time_values_m <- seq(0, 350, by = 10)     # Time span of Data in Days 

## Parameters  and Initial Value
parameters_values_m <- c(
  beta_m <- beta_net_m,      # This is where Beta will differ from original model     
  gamma <- 0.125
)
initial_values_f <- c(
  S = N_m - 1,     # Population of Susceptable Females in Venice At 1/03/1631
  I = 1,           # Number of Females infected on 01/03/1631
  R = 0            # Number of Females Recovered/Dead on 01/03/1631
)

## SIR Modeling 
sir_values_m <- ode(
  y = initial_values_m,
  times = time_values,
  func = sir_model_m,
  parms = parameters_values_m
 )
sir_values_m <- as.data.frame(sir_values_m)

## Plotting Female's SIR Model 
matplot(sir_values_m$time, sir_values_m[, c("S", "I", "R")],
        type = "l", lty = 1, col = c("blue", "red", "green"),
        xlab = "Time (Days)", ylab = "Male Population in Venice 1630")
legend("right", c("Susceptibles", "Infectious", "Recovered/Dead"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")

## Isolating the Infectious Pupulation 
plot(sir_values_m$time, sir_values_m$I, type = "l", col = "red",
     xlab = "Time (Days)", ylab = "Infectious Male Population 1630")
# R0 Calculation
(100000 + 1) * beta_net_f / 0.125
