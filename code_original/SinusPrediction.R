LSTM_sinus_prediction <-function(lag, StepPred,bs=1,ep=100,lstm1_units, scaledY,y ){
  
  # Test Data set
  test1 <- scaledY[1:(lag+StepPred)] # Lower End
  test2 <- scaledY[(length(scaledY)-lag-StepPred+1):length(scaledY)] # Upper End
  scaledY <- scaledY[(lag+1):(length(scaledY)-StepPred)] # Dataset without Test Data
  
  # Create Array for Feature and Label
  Feature <- FeatLabCreator(lag = lag , y = scaledY)[["Feature"]]
  Label <- FeatLabCreator(lag = lag, y = scaledY)[["Label"]]
  
  # Split Data into Train and Validation
  TrainValid_dat <- TrainValidTest(Feature = Feature, Label = Label, proportion = 0.8)
  
  # Model Prediction
  model <- keras_model_sequential()
  model %>%
    layer_lstm(units = lstm1_units,input_shape = c(lag, 1), return_sequences = F ) %>%
    layer_dense(units = 1)
  
  # Compile the Neural Network
  model %>%compile(
    loss = 'mean_squared_error',
    optimizer = "adam",
    metrics = c('mean_squared_error')
  )
  
  # Train the Neural Network
  set.seed(1234)
  history_model <- model %>% fit(TrainValid_dat$Feature_Train, TrainValid_dat$Label_Train,
                                 batch_size = bs, 
                                 epochs = ep, 
                                 shuffle = TRUE,
                                 verbose = TRUE, # 1shows,
                                 validation_data = list(TrainValid_dat$Feature_Validation,
                                                        TrainValid_dat$Label_Validation),
                                 callbacks = list(callback_reduce_lr_on_plateau(monitor = "val_loss",
                                                                                factor = 0.1,
                                                                                patience = 10),
                                                  callback_early_stopping(monitor = "val_loss",
                                                                          min_delta = 0,
                                                                          patience = 30)
                                 )
  )
  
  # Evaluation of predictions of the Neural Network on test set
  
  prediction1 <- pred_steps(model, start=test1[1:lag], StepPred=StepPred)
  prediction2 <- pred_steps(model, start=test2[1:lag], StepPred=StepPred)
  
  true1 <- test1[(lag+1):length(test1)]
  true2 <- test2[(lag+1):length(test2)]
  
  # backscale
  prediction1 <- prediction1*(max(y)-min(y)) + min(y)
  prediction2 <- prediction2*(max(y)-min(y)) + min(y)
  
  true1 <- true1*(max(y)-min(y)) + min(y)
  true2 <- true2*(max(y)-min(y)) + min(y)
  
  # mean squared error
  mse1 <- sum( (prediction1-true1)^2   )/length(prediction1)
  mse2 <- sum( (prediction2-true2)^2   )/length(prediction2)
  
  # Save Hyperparameters & Performance
  results <- list("results"=c("lag"=lag, "StepPred"=StepPred, "lstm1_units" =lstm1_units, "MSE1"=mse1,"MSE2"=mse2),"model"=model,"history"=as.data.frame(history_model))
  
  return(results)
}

