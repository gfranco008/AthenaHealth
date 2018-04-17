###########################################################################################*
# 
# Gustavo Franco Reynoso
#
#  Exponential Smoothing State Space model
#
# Calls auto ets function when frequency <= 24
# When frequency > 24, seasonality considered
#       ts: inputs ts data series
#       horizon: how many data points to forecast
#  returns a list containing:
#       fitted_values: fitted values in the period in the priod actual data is given 
#       pred_values: predicted values 
#
###########################################################################################*

ets_stlf <- function(ts, horizon) {

  # Unit tests
  assertive::assert_is_ts(ts)
  assertive::assert_all_are_positive(horizon)
  assertive::assert_all_are_false(anyNA(ts))

  frequency <- frequency(ts)
  
  if (frequency <= 24){
    model <- forecast::ets(ts, model = "ZZZ")
  } else {
    model <- forecast::stlf(ts, level = c(60,85,95))
  }
  
  fitted_values <- model
  pred_values <- forecast::forecast(model, h = horizon, level = c(60,85,95))
  
  return(list(fitted_values, pred_values))
}
