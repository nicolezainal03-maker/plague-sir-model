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
str(clean_df)

#----NETWORK ANALYSIS FOR FEMALES-------
# Isolate by Gender 
female_df <- clean_df %>%
  filter(!is.na(Date), Sex == 'F', plague_flag == 1, Days_Afflicted != "Unkown") %>%
  mutate(person_id_f = row_number())
female_df

# Time Window: 8 days 
k <- 8

## Edges for Females 
edges_f <- female_df %>%
  select(person_id_f, Date) %>%
  cross_join(
    female_df %>% select(person_id_f, Date),
    suffix = c("_from", "_to")
  ) %>%
  filter(person_id_f_from < person_id_f_to) %>%
  mutate(day_diff = abs(as.integer(Date_from - Date_to))) %>%
  filter(day_diff <= k)
# Edge Weight
g_f <- graph_from_data_frame(edges_f, directed = FALSE)
avg_degree_f <- mean(degree(g_f))
N_f <- sum(clean_df$Sex == "F")

# New Beta Term
beta_0 <- 0.1625
beta_net_f <- beta_0 * (1 + alpha * avg_degree_f / N_f)
beta_net_f

#----------NEW SIR MODEL-----------------##
# Things That wont Change
sir_model_f <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R                  
    dS <- -beta_f * S * I / N
    dI <-  beta_f * S * I / N - gamma * I
    dR <-  gamma * I
    
    list(c(dS, dI, dR))
  })
}
alpha <- 2
time_values_f <- seq(0, 350, by = 10)     # Time span of Data in Days 

## Parameters  and Initial Value
parameters_values_f <- c(
  beta_f <- beta_net_f,      # This is where Beta will differ from original model     
  gamma <- 0.125
)
initial_values_f <- c(
  S = N_f - 1,     # Population of Susceptable Females in Venice At 1/03/1631
  I = 1,           # Number of Females infected on 01/03/1631
  R = 0            # Number of Females Recovered/Dead on 01/03/1631
)

## SIR Modeling 
sir_values_f <- ode(
  y = initial_values_f,
  times = time_values,
  func = sir_model_f,
  parms = parameters_values_f
)
sir_values_f <- as.data.frame(sir_values_f)

## Plotting Female's SIR Model 
matplot(sir_values_f$time, sir_values_f[, c("S", "I", "R")],
        type = "l", lty = 1, col = c("blue", "red", "green"),
        xlab = "Time (Days)", ylab = "Female Population in Venice 1630")
legend("right", c("Susceptibles", "Infectious", "Recovered/Dead"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")

## Isolating the Infectious Pupulation 
plot(sir_values_f$time, sir_values_f$I, type = "l", col = "red",
     xlab = "Time (Days)", ylab = "Infectious Female Population 1630")
# R0 Calculation
(100000 + 1) * beta_net_f / 0.125

