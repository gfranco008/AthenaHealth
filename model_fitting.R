###########################################################################################*
# Athena Health
# Gustavo Franco Reynoso
# 04/09/2018
#
# Main model selection function selects the best forecasting method 
# Inputs:
#       smooth_ts: a time series object
#       model_select: model selection string, "Best" determines and outputs most accurate model
#       horizon: number of time points to forecast
# Ouputs:  
#       a list containing predictions 
#
###########################################################################################*



model_fitting <- function(smooth_ts, horizon, model_select = "Best") {
  #-----------------------------------------------------------------------------#
  ############################# Library Calls ###################################
  #-----------------------------------------------------------------------------#
  library(assertive)
  # library(forecast)
  
  #-----------------------------------------------------------------------------#
  ######################### Unit Test & Pre format ##############################
  #-----------------------------------------------------------------------------#
  options(warn=-1)
  assert_is_ts(smooth_ts)
  assert_all_are_positive(horizon)
  assert_is_identical_to_true(is.element(model_select, c("ARIMA", "ES", "AR1", "AR2", "stFF", "Best")))
  assert_all_are_false(anyNA(smooth_ts))
  
  
  #-----------------------------------------------------------------------------#
  ######################### Initialize Components ##############################
  #-----------------------------------------------------------------------------#
  # Currently available models
  # There's also model "st_ff" (season, trend fixed factor) which is only
  # available for individual selection, and only works for seasonal data
  
  models_suite <- c("ARIMA", "ES", "AR1", "AR2")
  
  # This function allows for easy switching among the different types of
  # models available
  model_function <- function(tseries, h, model) {
    switch(model,
           ARIMA = ARIMA(tseries, h),
           ES = ets_stlf(tseries, h),
           AR1 = AR1(tseries, h),
           AR2 = AR2(tseries, h ),
           stFF = st_ff(tseries, h))
  }
  
  # Initialize data frame of error metrics
  error_df <- data.frame(ME = double(),
                         RMSE = double(),
                         MAE = double(),
                         MPE = double(),
                         MAPE = double(),
                         MASE = double(),
                         ACF1 = double(),
                         MAAPE = double())
  
  # Initialize vector of models that failed to run on time series
  fail_models <- character()
  
  
  #-----------------------------------------------------------------------------#
  ##################### Forecasting with Specific Model #########################
  #-----------------------------------------------------------------------------#
  # If particular model is specified by user input (not default)
  if (model_select != "Best") {
    selected_model <- model_select  # Selected model by algorithm is model from user input
    
    model <- try(model_function(smooth_ts, horizon, model_select))
    
    # Check if model runs successfully
    if (class(model) == "try-error") {
      success <- FALSE
    } else {
      # Model fit and forecast data
      success <- TRUE
      values_pred <- fitted(model[[1]])
      values_fcast <- model[[2]]
      values_fcast <- values_fcast$mean
    }
    
  } else {
    
    #-----------------------------------------------------------------------------#
    ######################### Forecasting with Best Model #########################
    #-----------------------------------------------------------------------------#
    
    # No model is specified, all models are run, and the best is selected based on MAAPE
    index <- 1
    
    for (m in models_suite) {
      
      model <-  try(model_function(smooth_ts, horizon, m))
      
      if (class(model) == "try-error") {
        # If the selected model fails, skip it.
        # Do not consider it for voting later
        fail_models[length(fail_models) + 1] <- m
      } else {
        # Run error metrics on model
        model_error <- forecast::accuracy(model[[1]]) %>% as.data.frame()
        model_error$MAAPE <- switch(m,
                                    "ARIMA" = ,
                                    "AR1" = ,
                                    "AR2" = suppressWarnings((1/length(model[[1]][[17]]))*sum(atan(abs(model[[1]][[8]]/model[[1]][[17]])))),
                                    "ES" = if(frequency(smooth_ts) <= 24) {
                                      suppressWarnings((1/length(model[[1]][[1]]))*sum(atan(abs(model[[1]][[8]]/model[[1]][[1]]))))
                                    } else {
                                      (1/length(model[[2]]$x))*sum(atan(abs(model[[1]]$residuals/model[[2]]$x)))
                                    })
        
        error_df[index, names(model_error)] <- model_error[1, ]
        rownames(error_df)[index] <- m
        
        index <- index + 1
      }
    }
    
    
    #-----------------------------------------------------------------------------#
    ############################ Selecting Best Model #############################
    #-----------------------------------------------------------------------------#
    
    # Check to make sure all models didn't fail
    if (length(fail_models) != length(models_suite)) {
      
      # At least one model worked
      success <- TRUE
      
      # Selected model determined to be one with minimum MAAPE
      selected_model <- rownames(error_df)[which.min(abs(error_df$MAAPE))]
      
      model <- model_function(smooth_ts, horizon, selected_model)
      
      # Model fit and forecast data
      values_pred <- fitted(model[[1]])
      values_fcast <- model[[2]]
      values_fcast <- values_fcast$mean
    } else {
      # If all models failed
      success <- FALSE
    }
  }
  
  # Return a list of empty data if algorithm failed
  if (success == FALSE) {
    empty_ts <- smooth_ts * 0
    values_pred <- empty.ts
    
    # Resetting empty_ts so length is correct
    empty_ts <- ts(rep(0, horizon),
                   start = c(end(smooth_ts)[[1]], end(smooth_ts)[[2]] + 1), 
                   frequency = frequency(smooth_ts))
    values_fcast <- empty_ts
  }
  
  output_list <- list(selected_model, 
                      values_fcast)
  
  names(output_list) <- c("pred_type", 
                          "ForecastedValues")
  options(warn=0)
  return(output_list)
}

