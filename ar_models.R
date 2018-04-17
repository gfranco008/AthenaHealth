###########################################################################################*
# Gustavo Franco Reynoso
#
#
#' Parent AR function
#       ts: inputs ts data series
#       horizon: how many data points to forecast
#       p: p argument in (p, d, q)
#  returns a list containing:
#       fitted_values: fitted values in the period in the priod actual data is given 
#       pred_values: predicted values 
#
###########################################################################################*
ar_model <- function(ts, horizon, p) {

  # Unit tests
  assertive::assert_is_ts(ts)
  assertive::assert_all_are_positive(horizon)
  assertive::assert_all_are_non_negative(p)
  assertive::assert_all_are_false(anyNA(ts))

  # The version which works with fpp
  model <- forecast::Arima(ts, order = c(p, 0, 0), method = "ML")
  fitted_values <- model
  pred_values <- forecast::forecast(model, h = horizon, level = c(60,85,95))

  return(list(fitted_values, pred_values))
}

###########################################################################################*
# AR1 function
#       ts: input ts data series
#       horizon: how many data points to forecast
# returns a list containing:
#       fitted_values: fitted values in the period in the priod actual data is given 
#       pred_values: predicted values  
###########################################################################################*
AR1 <- function(ts, horizon){
  return(ar_model(ts, horizon, 1))
}
###########################################################################################*
# AR2 function
#       ts: input ts data series
#       horizon: how many data points to forecast
# returns a list containing:
#       fitted_values: fitted values in the period in the priod actual data is given 
#       pred_values: predicted values  
###########################################################################################*
AR2 <- function(ts, horizon){
  return(ar_model(ts, horizon, 2))
}
