
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
str(clean_df)

#----------MODELING BY AGE GROUP SET UP----------------
# Infant (Age < 1)
Infant_df <- clean_df %>%
  filter(!is.na(Date), age_group == "Infant", plague_flag == 1, Days_Afflicted != "Unkown") |>
  mutate(person_id_I = row_number())
Infant_df

#Child (1 < Age < 13)
Child_df <- clean_df %>%
  filter(!is.na(Date), age_group == "Child", plague_flag == 1, Days_Afflicted != "Unkown") |>
  mutate(person_id_C = row_number())
Child_df

# Teen (13 < Age < 20)
Teen_df <- clean_df %>%
  filter(!is.na(Date), age_group == "Teen", plague_flag == 1, Days_Afflicted != "Unkown") |>
  mutate(person_id_T = row_number())
Teen_df

# Young Adult (20< Age < 40)
Young_Adult_df <- clean_df %>%
  filter(!is.na(Date), age_group == "Young Adult", plague_flag == 1, Days_Afflicted != "Unkown") |>
  mutate(person_id_Y = row_number())
Young_Adult_df

# Middle Age (40 < Age < 60)
Middle_Age_df <- clean_df %>%
  filter(!is.na(Date), age_group == "Middle Age", plague_flag == 1, Days_Afflicted != "Unkown") |>
  mutate(person_id_M = row_number())
Middle_Age_df

# Older Adult (Age > 60)
Older_Adult_df <- clean_df %>%
  filter(!is.na(Date), age_group == "Older Adult", plague_flag == 1, Days_Afflicted != "Unkown") |>
  mutate(person_id_G = row_number())
Older_Adult_df

#----SIR MODELS BY AGE----------- 
##CONSTANTS##
k <- 8
alpha <- 2
beta_0 <- 0.1625
# Infant ##
edges_I <- Infant_df %>%
  select(person_id_I, Date) %>%
  cross_join(
    Infant_df %>% select(person_id_I, Date),
    suffix = c("_from", "_to")
  ) %>%
  filter(person_id_I_from < person_id_I_to) %>%
  mutate(day_diff = abs(as.integer(Date_from - Date_to))) %>%
  filter(day_diff <= k)
print(edges_I)
count(edges_I)


g_I <- graph_from_data_frame(edges_I, directed = FALSE)
avg_degree_I <- mean(degree(g_I))
N_m <- sum(clean_df$age_group == "Infant")
avg_degree_I


beta_net_I <- beta_0 * (1 + alpha * avg_degree_I / N_I)
beta_net_I

sir_model_I <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R                  
    dS <- -beta_i* S * I / N
    dI <-  beta_i * S * I / N - gamma * I
    dR <-  gamma * I
    list(c(dS, dI, dR))
  })
}


parameters_values_I <- c(
  beta_i <- beta_net_I,          
  gamma <- 0.125
)

initial_values_I <- c(
  S = N_I - 1,         
  I = 1,              
  R = 0              
)

time_values_I <- seq(0, 350, by = 10)    

sir_values_I <- ode(
  y = initial_values_I,
  times = time_values_I,
  func = sir_model_I,
  parms = parameters_values_I
)

sir_values_I <- as.data.frame(sir_values_I)
sir_values_I

matplot(sir_values_I$time, sir_values_I[, c("S", "I", "R")],
        type = "l", lty = 1, col = c("blue", "red", "green"),
        xlab = "Time (Days)", ylab = "Population of Infants in Venice 1630")
legend("right", c("Susceptibles", "Infectious", "Recovered/Dead"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")

plot(sir_values_I$time, sir_values_I$I, type = "l", col = "red",
     xlab = "Time (Days)", ylab = "Infectious Infant Population 1630")
#R0 
(100000 + 1) * beta_net_I / 0.125


# Child Model 
edges_C <- Child_df %>%
  select(person_id_C, Date) %>%
  cross_join(
    Child_df %>% select(person_id_C, Date),
    suffix = c("_from", "_to")
  ) %>%
  filter(person_id_C_from < person_id_C_to) %>%
  mutate(day_diff = abs(as.integer(Date_from - Date_to))) %>%
  filter(day_diff <= k)
print(edges_C)
count(edges_C)


g_C <- graph_from_data_frame(edges_C, directed = FALSE)
avg_degree_C <- mean(degree(g_C))
N_C <- sum(clean_df$age_group == "Child")
avg_degree_C


beta_net_C <- beta_0 * (1 + alpha * avg_degree_C / N_C)
beta_net_C

sir_model_C <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R                  
    dS <- -beta_C * S * I / N
    dI <-  beta_C * S * I / N - gamma * I
    dR <-  gamma * I
    list(c(dS, dI, dR))
  })
}


parameters_values_C <- c(
  beta_C <- beta_net_C,          
  gamma <- 0.125
)

initial_values_C <- c(
  S = N_C - 1,         
  I = 1,              
  R = 0              
)

time_values_C <- seq(0, 350, by = 10)    

sir_values_C <- ode(
  y = initial_values_C,
  times = time_values_C,
  func = sir_model_C,
  parms = parameters_values_C
)

sir_values_C <- as.data.frame(sir_values_C)
sir_values_C

matplot(sir_values_C$time, sir_values_C[, c("S", "I", "R")],
        type = "l", lty = 1, col = c("blue", "red", "green"),
        xlab = "Time (Days)", ylab = "Population in of Children Venice 1930")
legend("right", c("Susceptibles", "Infectious", "Recovered/Dead"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")

plot(sir_values_C$time, sir_values_C$I, type = "l", col = "red",
     xlab = "Time (Days)", ylab = "Infectious Children Population 1630")
#R0 
(100000 + 1) * beta_net_C / 0.125


# Teen Model 
edges_T <- Teen_df %>%
  select(person_id_T, Date) %>%
  cross_join(
    Teen_df %>% select(person_id_T, Date),
    suffix = c("_from", "_to")
  ) %>%
  filter(person_id_T_from < person_id_T_to) %>%
  mutate(day_diff = abs(as.integer(Date_from - Date_to))) %>%
  filter(day_diff <= k)
print(edges_T)
count(edges_T)


g_T <- graph_from_data_frame(edges_T, directed = FALSE)
avg_degree_T <- mean(degree(g_T))
N_T <- sum(clean_df$age_group == "Teen")
avg_degree_T


beta_net_T <- beta_0 * (1 + alpha * avg_degree_T / N_T)
beta_net_T

sir_model_T <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R                  
    dS <- -beta_T * S * I / N
    dI <-  beta_T * S * I / N - gamma * I
    dR <-  gamma * I
    list(c(dS, dI, dR))
  })
}


parameters_values_T <- c(
  beta_T <- beta_net_T,          
  gamma <- 0.125
)

initial_values_T <- c(
  S = N_T - 1,         
  I = 1,              
  R = 0              
)

time_values_T <- seq(0, 350, by = 10)    

sir_values_T <- ode(
  y = initial_values_T,
  times = time_values_T,
  func = sir_model_T,
  parms = parameters_values_T
)

sir_values_T <- as.data.frame(sir_values_T)
sir_values_T

matplot(sir_values_T$time, sir_values_T[, c("S", "I", "R")],
        type = "l", lty = 1, col = c("blue", "red", "green"),
        xlab = "Time (Days)", ylab = "Population in of Teens Venice 1930")
legend("right", c("Susceptibles", "Infectious", "Recovered/Dead"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")

plot(sir_values_T$time, sir_values_T$I, type = "l", col = "red",
     xlab = "Time (Days)", ylab = "Infectious Teen Population 1630")



# Young Adult Model 
edges_Y <- Young_Adult_df %>%
  select(person_id_Y, Date) %>%
  cross_join(
    Young_Adult_df %>% select(person_id_Y, Date),
    suffix = c("_from", "_to")
  ) %>%
  filter(person_id_Y_from < person_id_Y_to) %>%
  mutate(day_diff = abs(as.integer(Date_from - Date_to))) %>%
  filter(day_diff <= k)
print(edges_Y)
count(edges_Y)



g_Y <- graph_from_data_frame(edges_Y, directed = FALSE)
avg_degree_Y <- mean(degree(g_Y))
N_Y <- sum(clean_df$age_group == "Young Adult")
avg_degree_Y


beta_net_Y <- beta_0 * (1 + alpha * avg_degree_Y / N_Y)
beta_net_Y

sir_model_Y <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R                  
    dS <- -beta_Y * S * I / N
    dI <-  beta_Y * S * I / N - gamma * I
    dR <-  gamma * I
    list(c(dS, dI, dR))
  })
}


parameters_values_Y <- c(
  beta_Y <- beta_net_Y,          
  gamma <- 0.125
)

initial_values_Y <- c(
  S = N_Y - 1,         
  I = 1,              
  R = 0              
)

time_values_Y <- seq(0, 350, by = 10)    

sir_values_Y <- ode(
  y = initial_values_Y,
  times = time_values_Y,
  func = sir_model_Y,
  parms = parameters_values_Y
)

sir_values_Y <- as.data.frame(sir_values_Y)
sir_values_Y

matplot(sir_values_Y$time, sir_values_Y[, c("S", "I", "R")],
        type = "l", lty = 1, col = c("blue", "red", "green"),
        xlab = "Time (Days)", ylab = "Population in of Young Adults Venice 1930")
legend("right", c("Susceptibles", "Infectious", "Recovered/Dead"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")

plot(sir_values_Y$time, sir_values_Y$I, type = "l", col = "red",
     xlab = "Time (Days)", ylab = "Infectious Young Adult Population 1630")



# Middle Age Model 
edges_M <- Middle_Age_df %>%
  select(person_id_M, Date) %>%
  cross_join(
    Middle_Age_df %>% select(person_id_M, Date),
    suffix = c("_from", "_to")
  ) %>%
  filter(person_id_M_from < person_id_M_to) %>%
  mutate(day_diff = abs(as.integer(Date_from - Date_to))) %>%
  filter(day_diff <= k)
print(edges_M)
count(edges_M)


g_M <- graph_from_data_frame(edges_M, directed = FALSE)
avg_degree_M <- mean(degree(g_M))
N_M <- sum(clean_df$age_group == "Middle Age")
avg_degree_M


beta_net_M <- beta_0 * (1 + alpha * avg_degree_M / N_M)
beta_net_M

sir_model_M <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R                  
    dS <- -beta_M * S * I / N
    dI <-  beta_M * S * I / N - gamma * I
    dR <-  gamma * I
    list(c(dS, dI, dR))
  })
}


parameters_values_M <- c(
  beta_M <- beta_net_M,          
  gamma <- 0.125
)

initial_values_M <- c(
  S = N_M - 1,         
  I = 1,              
  R = 0              
)

time_values_M <- seq(0, 350, by = 10)    

sir_values_M <- ode(
  y = initial_values_M,
  times = time_values_M,
  func = sir_model_M,
  parms = parameters_values_M
)

sir_values_M <- as.data.frame(sir_values_M)
sir_values_M

matplot(sir_values_M$time, sir_values_M[, c("S", "I", "R")],
        type = "l", lty = 1, col = c("blue", "red", "green"),
        xlab = "Time (Days)", ylab = "Population in of Middle Ages Venice 1930")
legend("right", c("Susceptibles", "Infectious", "Recovered/Dead"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")

plot(sir_values_M$time, sir_values_M$I, type = "l", col = "red",
     xlab = "Time (Days)", ylab = "Infectious Middle Age Population 1630")

# Older Age Model 
edges_G <- Older_Adult_df %>%
  select(person_id_G, Date) %>%
  cross_join(
    Older_Adult_df %>% select(person_id_G, Date),
    suffix = c("_from", "_to")
  ) %>%
  filter(person_id_G_from < person_id_G_to) %>%
  mutate(day_diff = abs(as.integer(Date_from - Date_to))) %>%
  filter(day_diff <= k)
print(edges_G)
count(edges_G)



g_G <- graph_from_data_frame(edges_G, directed = FALSE)
avg_degree_G <- mean(degree(g_G))
N_G <- sum(clean_df$age_group == "Older Adult")
avg_degree_G


beta_net_G <- beta_0 * (1 + alpha * avg_degree_G / N_G)
beta_net_G

sir_model_G <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R                  
    dS <- -beta_G * S * I / N
    dI <-  beta_G * S * I / N - gamma * I
    dR <-  gamma * I
    list(c(dS, dI, dR))
  })
}


parameters_values_G <- c(
  beta_G <- beta_net_G,          
  gamma <- 0.125
)

initial_values_G <- c(
  S = N_G - 1,         
  I = 1,              
  R = 0              
)

time_values_G <- seq(0, 350, by = 10)    

sir_values_G <- ode(
  y = initial_values_G,
  times = time_values_G,
  func = sir_model_G,
  parms = parameters_values_G
)

sir_values_G <- as.data.frame(sir_values_G)
sir_values_G

matplot(sir_values_G$time, sir_values_G[, c("S", "I", "R")],
        type = "l", lty = 1, col = c("blue", "red", "green"),
        xlab = "Time (Days)", ylab = "Population in of Seniors Venice 1930")
legend("right", c("Susceptibles", "Infectious", "Recovered/Dead"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")

plot(sir_values_G$time, sir_values_G$I, type = "l", col = "red",
     xlab = "Time (Days)", ylab = "Infectious Senior Population 1630")

