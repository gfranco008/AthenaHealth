###########################################################################################*
# Gustavo Franco Reynoso
#
#
# ARIMA function
#       ts: input ts data series
#       horizon: how many data points to forecast
# returns list containing:
#       fitted_values: fitted values in the period in the priod actual data is given 
#       pred_values: predicted values 
#
###########################################################################################*


ARIMA <- function(ts, horizon) {

  # Unit tests
  assertive::assert_is_ts(ts)
  assertive::assert_all_are_positive(horizon)
  assertive::assert_all_are_false(anyNA(ts))

  model <- forecast::auto.arima(ts)
  fitted_values <- model
  pred_values <- forecast::forecast(model, h = horizon, level = c(60,85,95))

  return(list(fitted_values, pred_values))
}
