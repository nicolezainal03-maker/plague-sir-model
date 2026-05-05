# plague-sir-model
SIR epidemiological model of 17th-century plague outbreak in Venice, Italy.

# Overview
This project models the spread of the 1630–1631 Venice plague using a network-augmented SIR (Susceptible–Infected–Recovered) model. Unlike traditional SIR approaches that assume a constant transmission rate (β), this model introduces dynamic, structure-driven transmission by incorporating temporal contact networks derived from historical death records of a small parish inside Venice. The goal of this project is to understand how population structure, demographic variation, and connectivity influence epidemic spread.

# Data Set 
Data was gathered using historical death records from St. Eufimia parish (Venice) with about 800 entries. The data set includes specific demographics such as age, sex, cause of death, date of death, and duration of illness. The data is limited in that there are no counts of the full population during this time, there are missing infection timelines, and there are instances of inconsistent historical records. To reduce the impact of these limitations, death counts were used as a proxy for population, an infectious period of 8 days was constructed, and a temporal relationship between individuals was engineered. 

# Methodology 
Firstly, the data was cleaned and standardized in R, a time index was created (days_since_start), and causes of death were grouped into plague vs. non-plague categories. Next, a standard SIR model was implemented using deSolve, which defined β (transmission rate) or beta, and γ (recovery rate) or gamma. Using a basic population size and transmission rate of the time (found via research), the baseline beta term significantly underestimated the spread, leading to limited infection propagation. It could be concluded from this that the beta is what would need to be varied. This leads to the network-based transmission model. A temporal contact network was constructed where nodes were the individuals, and edges were potential transmission links (with the condition that individuals could only be linked if death dates fell within an 8-day infection window). Igraph was used to compute the node degrees and analyze the connectivity structure. This was used to replace the constant beta term with degree-adjusted transmission rates. The higher the connectivity, the higher the effective transmission probability, which improves the realism of the spread dynamics. The data was validated by using an R0 term (basic reproduction number). Finally, a comparison was made between the baseline SIR model and the network-adjusted model, as well as the spread behavior and sensitivity to structure. 

# Conclusions 
It was found that standard SIR models have the potential to underestimate transmission in structured populations. Socio-economic demographics influence individuals' network connectivity, which plays a crucial role in epidemic dynamics. This demographic segmentation reveals heterogeneous spread patterns. Lastly, temporal overlap modeling significantly improves realism

# Code Features
This project includes visualizations: infection curves (SIR trajectories), age distribution of plague deaths, gender-based mortality comparison, and network connectivity structure
The Libraries used in R were: tidyverse, igraph, deSolve, ggplot2. For Data cleaning & transformation, network modeling, and simulation modeling





