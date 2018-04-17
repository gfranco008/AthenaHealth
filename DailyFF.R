###########################################################################################*

# Gustavo Franco Reynoso
#
# Fixed effect dynamic regression, use for daily forecasts
#       ts: input ts data series
#       horizon: how many data points to forecast
# returns a list containing:
#       fitted_values: fitted values in the period in the priod actual data is given 
#       pred_values: predicted values  
###########################################################################################*


DailyFF <- function(ts, horizon) {
  
  # Unit tests
  assertive::assert_is_ts(ts)
  assertive::assert_all_are_positive(horizon)
  assertive::assert_all_are_false(anyNA(ts))

  dates <- ts %>% time() %>% as.numeric() %>% lubridate::date_decimal() %>% lubridate::month()
  values <- ts %>% as.numeric()
  
  df <- data.frame(
    Date   = dates,
    Amount = values,
    Year   = dates %>% lubridate::year(),
    Month  = dates %>% lubridate::month(),
    Week   = dates %>% lubridate::week(),
    Day    = dates %>% lubridate::day(),
    DOW    = dates %>% lubridate::wday(),
    Order  = 1:length(dates)
  )
  
  # Create day of week dummies
  DOW <- df$DOW %>% factor()
  dummies.dow <- (~DOW + 0) %>% model.matrix()
  
  # Create month dummies
  Month <- df$Month %>% factor()
  dummies.month <- (~Month + 0) %>% model.matrix()
  
  # Create week dummies
  Week <- df$Week %>% factor()
  dummies.week <- (~Week + 0) %>% model.matrix()
  
  # n-1 dummies for one degree of freedom
  # remove date as it's no longer relevant
  df <- cbind(df, dummies.dow, dummies.month, dummies.week)
  
  # Remove from the dataset: original date, last day of week, last month of
  # year, last week of year
  df <- dplyr::select(df, -c(Date, Month:DOW, DOW7, Month12))
  max_week <- Week %>% levels() %>% as.numeric() %>% max()


  df <- dplyr::select(df, -max_week)
  
  # Create df as ts
  startdate   <- c(year(dates[1]), month(dates[1]), day(dates[1]))
  
  # This needs to be global, otherwise the following function fails
  ts.combined <<- ts(df, frequency=frequency(ts), start=startdate)
  
  # Fit the time series linear model with fixed effects (this is also known as Fixed Factor Analysis)
  model <- forecast::tslm(Amount ~ ts.combined[, 2:ncol(ts.combined)], data = ts.combined)
  #accuracy(fittest)
  #summary(fittest)
  fitted_values <- model
  pred_values <- forecast::forecast(model, h = horizon, level = c(60,85,95))
  
  # Remove the global object just in case
  rm("ts.combined", pos=".GlobalEnv")
  
  return(list(fitted_values, pred_values))
}