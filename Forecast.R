
###########################################################################################*
# Athena Health
# Gustavo Franco Reynoso
# 04/12/2018
#
# Main model selection function selects the best forecasting method 
# Inputs:
#       data: dataframe containing needed information
#       horizon: number of points to forecast
#       measure: name of the desired measure to filter by
#       state: state to filter by
# Ouputs:  
#       a list containing original data and predictions 
#
###########################################################################################*



Forcast <- function(data, horizon, measure = c(), state = "MA" ) {
  
  #-----------------------------------------------------------------------------#
  ############################### Library Calls #################################
  #-----------------------------------------------------------------------------#
  library(assertr)
  library(assertive, warn.conflicts=F)
  library(dplyr, warn.conflicts=F)
  
  
  #-----------------------------------------------------------------------------#
  ######################### Unit Test & Pre format ##############################
  #-----------------------------------------------------------------------------#
  options(warn=-1)
  horizon %>% assert_is_a_number()
  horizon %>% is_positive()
  assert_all_are_true(inherits(data, "data.frame"))
  
  # Organize the DF
  if (is.null(measure) == TRUE){
  }else{
  data <- data %>% as.data.frame() %>% filter(`Measure Code` == measure) 
  }
  data <- data %>% as.data.frame() %>% filter(`Provider State` == state) 
  datap <- data %>% aggregate( by = list(data$`Provider Name`), mean) %>% 
    select(1,2,5,7,8,9,10) %>% na.omit()
  #-----------------------------------------------------------------------------#
  ############################### Pre allocation ################################
  #-----------------------------------------------------------------------------#
  tsdata <- list()
  size <- dim(datap)
  dataf <- matrix(0, size[1], (size[2]+ horizon)) %>% as.data.frame()
  colnames(dataf)[1:size[2]] <- names(datap) # Assign name
  
  #-----------------------------------------------------------------------------#
  ######################### Forecasting Next Quarters ###########################
  #-----------------------------------------------------------------------------#
  for (m in 1:nrow(datap)){
    # create a time series
    tsdata <- ts(t(datap[m,c(4:7)]),start=c(2016,4),end=c(2017,3),frequency=4)
    dataf[m,] <- datap[m,] %>% cbind(model_fitting(tsdata, horizon)[[2]] %>% as.data.frame() %>% t())
  }
  dataf <- dataf %>% rename(Forecast = V8)
  options(warn=0)
  return(dataf)
}
