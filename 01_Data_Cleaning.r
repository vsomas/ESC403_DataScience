
########################################################################
# Data Analysis
# Nikolai Horozov, Jerome Sepin, Vithersan Somasundaram and Gaby Stout
########################################################################

# Preparation ####

# Clear the desk
  # rm(list=ls())

# Load libraries
library(tidyverse)
library(ggpubr)
library(COVID19)      # Consist the COVID2019 data 
                      # Codebook can be found : https://covid19datahub.io/articles/r.html
library(tableone)     # for Descriptive Statistic
library(pastecs)      # for tableone summary
library(naniar)       # Visualizing missing values
library(visdat)

# Import the data
  #dd <- covid19(c("Switzerland",),level = 2, end = "2022-03-01", amr = TRUE)
dd <- covid19(c("Switzerland","Italy","Germany","Austira"),level = 1, amr = FALSE) 
  # level : 1 country-level, 2 state-level
  # amr : Apple Mobility Report ## codebook can be found: https://covid19.apple.com/mobility


# Tidy Data ####
## calculate daily cases and proportions
for (i in 2:nrow(dd)){                                                             
  dd$daily[i]  <- dd$confirmed[i] - dd$confirmed[i - 1]
  dd$prop[i]   <- dd$daily[i] / dd$population[i]
  dd$per100[i] <- dd$prop[i] * 100000 
  dd$dailydeaths[i]  <- dd$deaths[i] - dd$deaths[i - 1]
}

# Descriptive Statistics ####
 # Check for NAs:
  vis_dat(dd)   # provides information about the class of the data input into R, as well as whether the data is missing or not.
  vis_miss(dd)   # provides a summary of whether the data is missing or not.


  

