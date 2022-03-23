####################################################################
#####     Script to extract the precipitation time series       ####
#####  at a 10min time resolution from the dissagregated prec.  ####
#####   raster files, for each of the polygons of interest.     ####
#####           By Jovan Blagojevic, October 2020               ####
####################################################################

#######################################            
# Clear workspace and load libraries #----    
rm(list=ls())
library(sp)
library(rgdal)
library(raster)
library(plyr)
library(dplyr)
#library(velox)
library(sf)
#install.packages("ncdf4")
library(ncdf4)
#install.packages("TimeWarp")  
library(TimeWarp) 
library(stringi)
library(lubridate)
#install.packages("data.table")
library(data.table)
library(zoo)
setwd("Z:/CH_VOL_LANAT_EO/03_Data/Data/Reference_polygons_data/Complete_Dataset")

#Load the mannualy created erosion polygons
erosion_pol <- readOGR(dsn=".",layer="Complete_dataset_UTM")
random_pol <- readOGR(dsn=".",layer="Random_polygons_UTM")
#crs(erosion_pol)


###########################----
#Load the precipitaion data
setwd("Z:/CH_VOL_LANAT_EO/03_Data/Data/Precipitation/Station_data/Erosion_events_analysis")
files <- list.files(path = getwd(), full.names = FALSE,recursive = FALSE) 

#Disagregated precipitation files
prec_files_disag <- files[grep("Prec_disag", files)]
prec_files_disag <- prec_files_disag[-grep(".tif.", prec_files_disag)]
#Sort according to date
disag.dates <- substr(prec_files_disag,12,19 )
disag.dates.d <- as.POSIXct(disag.dates, format = "%Y%m%d") #date format
prec_files_disag <- prec_files_disag[order(disag.dates.d)]

#Interpolated precipitation files
prec_files_interp <- files[grep("Prec_interp", files)]
prec_files_interp <- prec_files_interp[-grep(".tif.", prec_files_interp)]
#Sort according to date
interp.dates <- substr(prec_files_interp,13,20 )
interp.dates.d <- as.POSIXct(interp.dates, format = "%Y%m%d") #date format
prec_files_interp <- prec_files_interp[order(interp.dates.d)]



#Daily sums of station interpolated precipitation
prec_files_daily <- files[grep("Dail_sum_interp", files)]
#Sort according to date
daily.dates <- substr(prec_files_daily,17,24 )
daily.dates.d <- as.POSIXct(daily.dates, format = "%d%m%Y") #date format
prec_files_daily <- prec_files_daily[order(daily.dates.d)]



setwd("Z:/CH_VOL_LANAT_EO/03_Data/Data/Precipitation/Station_data/Erosion_events_analysis")

#Project polygons to the crs of the precipitation files 
prec1 <- stack(prec_files_interp[8])
prec2 <- stack(prec_files_disag[8])
prec3 <- stack(prec_files_daily[8])
crs(prec1)
crs(prec2)
crs(prec3)
crs(erosion_pol)
crs(random_pol)
rm(list=(paste("prec", seq(1,3), sep="")))

prec <- prec_files_interp[8] #Any number works, use for reference CRS
prec_stack <- stack(prec)

erosion_pol <- spTransform(erosion_pol, crs(prec_stack))
random_pol <- spTransform(random_pol, crs(prec_stack))

rm(prec_stack, files, prec)

#Convert polygons into points - used for extracting prec. values
erosion_pol_points <-  SpatialPointsDataFrame(coords=coordinates(erosion_pol)
                                              ,data=erosion_pol@data)
random_pol_points <-  SpatialPointsDataFrame(coords=coordinates(random_pol)
                                              ,data=random_pol@data)


 #Create empty data frames for storing extracted prec data
df.extract.prec.disag <- data.frame(matrix(ncol = length(erosion_pol@polygons)+1, nrow = 0))
df.extract.prec.interp <- data.frame(matrix(ncol = length(erosion_pol@polygons)+1, nrow = 0))
df.extract.prec.daily <- data.frame(matrix(ncol = length(erosion_pol@polygons)+1, nrow = 0))

df.extract.prec.rand.disag <- data.frame(matrix(ncol = length(random_pol@polygons)+1, nrow = 0))
df.extract.prec.rand.interp <- data.frame(matrix(ncol = length(random_pol@polygons)+1, nrow = 0))
df.extract.prec.rand.daily <- data.frame(matrix(ncol = length(random_pol@polygons)+1, nrow = 0))

df.extract.prec.add.disag <- data.frame(matrix(ncol = length(erosion_pol@polygons)+1, nrow = 0))
df.extract.prec.add.interp <- data.frame(matrix(ncol = length(erosion_pol@polygons)+1, nrow = 0))

df.extract.prec.add.rand.disag <- data.frame(matrix(ncol = length(random_pol@polygons)+1, nrow = 0))
df.extract.prec.add.rand.interp <- data.frame(matrix(ncol = length(random_pol@polygons)+1, nrow = 0))

#################################################################################################
#Select the daily prec. raster stack files within a year and then extract
#the 10min prec. values at the locations of the polygons. The extracted time series
#are stored in tables, separately for each year.
# 215:428 - 2017
# 429:641 - 2018
# 642:855 - 2019
# 856:1024 - 2020
#Then name the output tables accordingly (set the year in rows 178 and onwards)!
#################################################################################################

for(prec_files_disag.extr in prec_files_disag[856:1024]){
  
  disag.dates <- substr(prec_files_disag.extr,12,19 )
  find.date.d <- as.POSIXct(disag.dates, format = "%Y%m%d") #date format
  find.date <- as.character(find.date.d, format = "%Y%m%d") #character for the 10min res. files
  find.day <- as.character(as.POSIXct(find.date.d, format = "%Y/%m/%d"), format = "%d%m%Y") #character for the daily sum files
  
  prec.disag.in <- stack(prec_files_disag.extr)
  prec.disag <- subset(prec.disag.in, 1:144)
  prec_files_interp.extr <- prec_files_interp[grep(find.date, prec_files_interp)]
  prec.interp.in <- stack(prec_files_interp.extr)
  prec.interp <- subset(prec.interp.in, 1:144)
  
  
  select.prec.files <- c("disag", "interp")
  
  start.time <- as.character(as.POSIXct(find.day, format = "%d%m%Y")+hours(6), format = "%Y%m%d%H%M")
  end.time <- as.character(as.POSIXct(find.day, format = "%d%m%Y")+hours(30)-minutes(10), format = "%Y%m%d%H%M")
  plot.times <- format(seq(strptime(start.time, format = "%Y%m%d%H%M"),
                           strptime(end.time, format = "%Y%m%d%H%M"),
                           by = as.difftime("00:10:00")),
                       format = "%Y%m%d%H%M")
  
  print(prec_files_disag.extr)
  print(Sys.time())
  
  for (i in select.prec.files){
    #i <- select.prec.files[2]
    
    file.name <- paste("prec", i, sep=".")
    df.name.add <- paste("df.extract.prec.add", i, sep=".")
    dat.add <- get(df.name.add) #load the df
    df.name <- paste("df.extract.prec", i, sep=".")
    dat <- get(df.name) #load the df
    
    df.name.add.rand <- paste("df.extract.prec.add.rand", i, sep=".")
    dat.add.rand <- get(df.name.add.rand) #load the df
    df.name.rand <- paste("df.extract.prec.rand", i, sep=".")
    dat.rand <- get(df.name.rand) #load the df
    
  for (j in 1:length(eval(as.symbol(file.name))@layers)){
    dat.add[j,101] <- plot.times[j]
    dat.add[j,1:100] <- extract(x = eval(as.symbol(file.name))@layers[[j]], y=erosion_pol_points, method='bilinear')
    assign(df.name.add, dat.add) #update with added data
    
    dat.add.rand[j,47] <- plot.times[j]
    dat.add.rand[j,1:46] <- extract(x = eval(as.symbol(file.name))@layers[[j]], y=random_pol_points, method='bilinear')
    assign(df.name.add.rand, dat.add.rand) #update with added data
  }
    
    updated.df <- rbind(eval(parse(text=df.name)), eval(parse(text=df.name.add)))
    assign(df.name, updated.df)
    
    updated.df.rand <- rbind(eval(parse(text=df.name.rand)), eval(parse(text=df.name.add.rand)))
    assign(df.name.rand, updated.df.rand)
  }
  
}

getwd()
 write.table(df.extract.prec.disag, file="Extracted_prec_time_series_disag_2020")
 write.table(df.extract.prec.interp, file="Extracted_prec_time_series_interp_2020")
 
 write.table(df.extract.prec.rand.disag, file="Extracted_prec_time_series_rand_disag_2020")
 write.table(df.extract.prec.rand.interp, file="Extracted_prec_time_series_rand_interp_2020")
 
 
 ###################################################################################
 #The following code lines served only for ploting and reviewing the generated data 
 ###################################################################################
# plot(prec_stack[[7]])
# plot(erosion_pol_points[34,], add=T)
# 
# df.extract.prec.disag.sum <- colSums(df.extract.prec.disag)
# df.extract.prec.interp.sum <- colSums(df.extract.prec.interp)
# df.extract.prec.daily.sum <-  colSums(df.extract.prec.daily)
# 
# plot(df.extract.prec.daily.sum)
# plot(df.extract.prec.disag.sum)
# plot(df.extract.prec.interp.sum)
# 
# 
# df.extract.prec.disag <- read.table("Extracted_prec_time_series_disag1")
# df.extract.prec.interp <- read.table("Extracted_prec_time_series_interp1")
# df.extract.prec.daily <- read.table("Extracted_prec_time_series_daily1")
# 
# #Execute given function over each matrix element
# emax <- function(x){
#   29*(1-0.60*exp(-0.061*x))*ceiling(x/100000000)
# }
# df.extract.prec.disag.kinetic <- apply(df.extract.prec.disag*6, c(1,2), emax)
# df.extract.prec.disag.kinetic[is.na(df.extract.prec.disag.kinetic)] <- 0
# 
# plot(29*(1-0.60*exp(-0.061*seq(0,200)))*ceiling(seq(0,200)/100000000), log="x")
# 
# for (i in 1:145){
#   browser()
#   plot(prec.interp[[i]])
# }
# 
# for (i in 1:100){
#   tryCatch({
#   browser()
#     dev.off()
#     par(mfrow=c(1,5))
#     print(i)
#   prec.max <- max(c(max(df.extract.prec.disag[,i], na.rm=T), max(df.extract.prec.interp[,i], na.rm=T)))
#   x.time=as.POSIXct(seq(strptime(erosion_pol@data$DATUM[i], format = "%Y/%m/%d")+hours(30),
#                         strptime(erosion_pol@data$DATUM[i], format = "%Y/%m/%d")+hours(30)-days(8),
#                         by = -as.difftime("00:10:00")), format = "%m%d%HH")
#   plot(x = x.time, y = df.extract.prec.disag[,i], typ="h", col="blue", main="Rhires/prelimD. disag.",
#        ylab="mm/10min", ylim=c(0,prec.max))
#   points(x = x.time, y = df.extract.prec.disag.kinetic[,i]/10)
#     plot(x = x.time, y = df.extract.prec.interp[,i], typ="h", col="red", main=paste("Stat. data interp.", erosion_pol@data$DATUM[i]),
#        ylim=c(0,prec.max))
#   plot(df.extract.prec.daily[,i], type="b", main="Stat. data interp.")
#   plot(x = rev(x.time), y =cumsum(rev(df.extract.prec.disag.kinetic[,i])), type="b", main="Cumulative kinetic energy")
#   plot(rollapply(df.extract.prec.daily[,i], 4, sum), ylim=c(0,max(rollapply(df.extract.prec.daily[,i], 5, sum))), xlab="Days before the <event>")
#   },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# }
# 
# plot(rollapply(df.extract.prec.daily[,i], 4, sum), ylim=c(0,max(rollapply(df.extract.prec.daily[,i], 5, sum))), xlab="Days before the <event>")
# 
# 
# 
# 
# 
# plot(x=as.POSIXct(seq(strptime(erosion_pol@data$DATUM[i], format = "%Y/%m/%d")+hours(30),strptime(erosion_pol@data$DATUM[i], format = "%Y/%m/%d")+hours(30)-days(8), 
#                       by = -as.difftime("00:10:00")), format = "%m%d%HH"),y=df.extract.prec.interp[,i]*6-0.8, typ="h", col="blue", main="Rhires/prelimD. disag.",
#      ylab="mm/10min", ylim=c(0,prec.max))
# 
# test<- c(df.extract.prec.interp[,i]*6-0.8)
# test[test<0]<- 0
# plot(cumsum(test))
