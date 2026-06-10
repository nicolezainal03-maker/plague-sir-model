install.packages("deSolve")
library(tidyverse)
library(deSolve)


## Epidemic Model ##
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R                  
    dS <- -beta * S * I / N
    dI <-  beta * S * I / N - gamma * I
    dR <-  gamma * I
    
    list(c(dS, dI, dR))
  })
}

## Assign Initial Values and Parameters##
parameters_values <- c(
  beta <- 0.1625,       # Infectious Contact Rate
  gamma <- 0.125        # Recovery/Death Rate 
)

initial_values <- c(
  S = 100000,         # Estimated Population of People in Venice
  I = 1,              # Number of People infected on day 1 
  R = 0               # Number of People Who are Recovered/Dead on  day 1 
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


## Converting to Data Fram for Visualization
sir_values <- as.data.frame(sir_values)
sir_values

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
(100000 + 1) * 0.1625 / 0.125

