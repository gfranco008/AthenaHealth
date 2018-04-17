###########################################################################################*
# 
# Gustavo Franco Reynoso
#
# st_ff function
#       ts: inputs ts data series
#       horizon: how many data points to forecast
#  returns a list containing:
#       fitted_values: fitted values in the period in the priod actual data is given 
#       pred_values: predicted values 
#
###########################################################################################*
st_ff <- function(ts, horizon) {
  
  # Unit tests
  assertive::assert_is_ts(ts)
  assertive::assert_all_are_positive(horizon)
  assertive::assert_all_are_false(anyNA(ts))

  model <- forecast::tslm(ts ~ trend + season)
  fitted_values <- model
  pred_values <- forecast::forecast(model, h = horizon, level = c(60,85,95))
  
  return(list(fitted_values, pred_values))
}