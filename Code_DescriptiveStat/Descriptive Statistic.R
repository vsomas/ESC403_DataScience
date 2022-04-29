#--------------------------------------------------------------------------------------------
# title:  Descriptive Statistic
# date:   26.04.2022
# author: Vithersan Somasundaram
#--------------------------------------------------------------------------------------------

# Preparation ####
# Clear the desk
rm(list=ls())

# Load data:
dd <- read.csv("./data/pollution.csv")

# Load Librarires:
library(tidyverse)
library(ggpubr)
library(forecast)

# tidy data
dd$cbwd <- factor(dd$cbwd,
                  levels = c("cv", "NE", "NW", "SE"),
                  labels = c("CV","NE","NW","SE"))

# Visualisation Response variable ####
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



# Introduction | Simulation visualising predicted future error  ####
start <- dd$TEMP[2:50]
for(i in 1:20){
  start <- c(start, start[length(start)] + rnorm(n = 1, mean = 0, sd = .5))
}

par(mfrow=c(1,2))
plot(dd$TEMP[2:100],type = "l", ylab = "Temperature [C°]", main = "" )
lines(dd$TEMP[1:99]+ rnorm(n = 99, mean = 0, sd = .5),type = "l", col= 'darkred')
legend("bottomleft", inset = .02, legend=c("Real Values", "Predicted Values"),
       col=c("black", "darkred"), lty=c(1,1), cex=0.5, bty = "n")

plot(dd$TEMP[2:100],type = "l", ylab = "Temperature [C°]", main = "" )
lines(start, col = "darkblue")
abline(v = 50, lty = 2)
legend("bottomleft",inset = .02, legend=c("Real Values", "Predicted Values"),
       col=c("black", "darkblue"), lty=c(1,1), cex=0.5, bty = "n")




dd <- dplyr::select(dd, year,month, TEMP)
dat <- array(data = c(unlist(dd)),
             dim = c(43824,3,3),
             dimnames = list(rownames(dd$year),
                             colnames(dd$month)))
ts.dat <- ts(dd$TEMP,start = c(2010), end=c(2014), frequency =24*365) #8760 / 24
plot(ts.dat, ylab = "Temperature [C°]", main = "Meterological Records in Beijjing" )

ts.dat <- ts(dd$TEMP,start = c(2010), end=c(2014), frequency =24)
plot(ts.dat)
