
#=======================================================================
# Data Preparation, COVID19 Data                                       #
# author: Vithersan Somasundaram                                       #
# Date: 23.03.2022                                                     #
#=======================================================================

# Preparation ####

# Clear the desk
   rm(list=ls())

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
  # Fixed until 01 March 2022
#dd <- covid19(level = 1, end = "2022-03-01", amr = FALSE) # Loading the whole dataset
                                      # Note: level : 1 country-level, 2 state-level
dd <- covid19(c("Switzerland","Zimbabwe"),level =1 , end = "2022-03-01", amr = TRUE) # Only Switzerland and Zimbabwe

# Tidy Data ####
  # get rid of unwanted variables:
dput(names(dd))
keeps <- c("id", "date", "confirmed", "deaths", "recovered", "tests", 
           "vaccines", "people_vaccinated", "people_fully_vaccinated", "hosp", 
           "icu","administrative_area_level_1","latitude", "longitude", "population", "iso_alpha_3", "iso_alpha_2")
dd <- dd[keeps]

## Calculaiton of daily cases and proportions:
for (i in 2:nrow(dd)){                                                             
  dd$daily[i]  <- dd$confirmed[i] - dd$confirmed[i - 1]
  dd$prop[i]   <- dd$daily[i] / dd$population[i]
  dd$per100[i] <- dd$prop[i] * 100000 
  dd$dailydeaths[i]  <- dd$deaths[i] - dd$deaths[i - 1]
}

#save to rda file 

#setwd("./data")
saveRDS(dd, file = "COVID19.rds")
