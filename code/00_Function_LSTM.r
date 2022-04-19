# Loading required Packages
require(keras)
require(tensorflow)
require(tidyverse)

# Scaling: 
scaling <- function(y){
  # Y = Response Variable:
  y_scaled <- (y-min(y))/(max(y)-min(y)) # Scaling Response funciton
  return(y_scaled)
}


# Features and Label creator: 
FeatLabCreator <- function(lag, y){
  dataX = c()
  dataY = c()

  for(i in 1:(length(y_scaled)-lag)){
    dataX <- c(dataX, y_scaled[i:(i+lag-1)])    # Feature
    dataY <- c(dataY, y_scaled[i+lag])          # Label
  }
  dataX <- matrix(dataX, ncol = lag, byrow = T) # Feature as Matrix
  dataX <- array(dataX, dim = c(nrow(dataX), lag, 1))
  dataY <- array(dataY, dim = c(nrow(dataX), 1, 1))
  return(list("Feature" = dataX,"Label" = dataY))
}



TrainValidTest <- function(Feature,Label, proportion){
  index <- as.integer(nrow(Feature)*proportion)                   # Proportion
  
    # Splitting Feature into Train set, Validaton set:
  Train_Feature <- Feature[1:index , ,]                           # Train set
  Train_Feature <- array(Train_Feature, dim = c(nrow(Train_Feature), ncol(Train_Feature), 1))
  
  Validation_Feature <- Feature[(index+1):nrow(Feature) , ,]      # Validation set
  Validation_Feature <- array(Validation_Feature, dim = c(nrow(Validation_Feature), ncol(Validation_Feature), 1))
  
    # Splitting Label into Train set, Validation set:
  Train_Label <- Label[1:index , ,]                               # Train set
  Train_Label <- array(Train_Label, dim = c(length(Train_Label), 1,1))
  Validation_Label <- Label[(index+1):nrow(Feature) , ,]          # Validation set
  Validation_Label <- array(Validation_Label, dim = c(length(Validation_Label), 1,1))
  
  return(list("Feature_Train" = Train_Feature, 
              "Feature_Validation" = Validation_Feature,
              "Label_Train" = Train_Label,
              "Label_Validation" = Validation_Label))
}
