#--------------------------------------------------------------------------------------------
# title:  Descriptive Statistic
# date:   26.04.2022
# author: Vithersan Somasundaram
#--------------------------------------------------------------------------------------------

# Clear the desk
rm(list=ls())

# Load data:
dd <- read.csv("./data/pollution.csv")

# Load Librarires:
library(tidyverse)
library(ggpubr)
library(naniar)       # Overview of the data
library(tableone)


# tidy data
dd$cbwd <- factor(dd$cbwd,
                  levels = c("cv", "NE", "NW", "SE"),
                  labels = c("CV","NE","NW","SE"))

# Overview of the data:
dd_miss <- dd
colnames(dd_miss) <-c("No", "Year", "Month", "Day", "Hour", 
                      "Particulate matter 2.5 µm", "Dew point", "Temperature", 
                      "Air pressure", "cbwd", "Iws", "Is", "Ir")

gg_miss_var(dd_miss, show_pct = TRUE)+
  theme_linedraw()+
  theme(axis.text.y = element_text(size = 10))


# Characterisitcs of the variables
variable = c("pm2.5", "DEWP", "TEMP", 
"PRES", "cbwd", "Iws", "Is", "Ir")
TableOne <- CreateTableOne(vars = variable, data = dd)
TableOne <- print(TableOne, showAllLevels = TRUE)
TableOne %>%
  kbl()%>%
  kable_classic(full_width = FALSE, html_font = "Cambria", position = "center")


# Visualisation Response variable
ggplot(data = dd, aes(x = seq(from = 1, to = length(dd$No), by= 1),y = TEMP))+
  geom_line(alpha = .5, colour = "black")+
  labs(x = "Observations", y = "Temperature")+
  #2010
  annotate("segment", 
           x = 0, xend = 0,
           y = -20, yend = 50, colour = "darkblue")+
  #2011
  annotate("segment", 
           x = 8764.8, xend = 8764.8,
           y = -20, yend = 50, colour = "darkblue")+
  #2012
  annotate("segment", 
           x = 17529.6, xend = 17529.6,
           y = -20, yend = 50, colour = "darkblue")+
  #2013
  annotate("segment", 
           x = 26294.4, xend = 26294.4,
           y = -20, yend = 50, colour = "darkblue")+
  #2014
  annotate("segment", 
           x = 35059.2, xend = 35059.2,
           y = -20, yend = 50, colour = "darkblue")+

  # Label
  annotate("text", x = c(4382.4,13147.2,21912,30676.8,39441.6), y = 45, label = c("2010","2011","2012","2013","2014"))+
  theme_linedraw()
