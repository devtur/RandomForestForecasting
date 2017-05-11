#' Calculate stochastic %K indicator
#'
#' @param stock_data stock data 
#' @param end_date end of the time horizon
#' @param K_days time horizon
#'
#' @return value of %K
calculate_K <- function(stock_data, end_date, K_days) {
  # Start date
  rnames = row.names(as.data.frame(stock_data))
  start_date = rnames[which(rnames == end_date) - K_days]
  
  current_close = stock_data[end_date, 4] # 4th column is close price
  
  lowest_low = min(stock_data[str_c(cbind(start_date, end_date), collapse = "/"), 3]) # 3rd column is low price
  highest_high = max(stock_data[str_c(cbind(start_date, end_date), collapse = "/"), 2]) # 2nd column is high price
  
  K = (current_close - lowest_low) / (highest_high - lowest_low) * 100
  
  K = as.numeric(K)
  
  return (K)
}

#' Calculate stochastic %D indicator
#'
#' @param stock_data stock data
#' @param date date at which to calculate
#' @param K_days time for horizon for %K indicator
#' @param MA_days number of days for moving average
#'
#' @return value of %D
calculate_D <- function(stock_data, date, K_days, MA_days) {
  # Dates from stock data
  rnames = row.names(as.data.frame(stock_data))
  
  # Get index for dates
  row = which(rnames == date)
  
  # Calculate Moving Average in K
  sum = 0
  for(i in (MA_days - 1):0) {
    # End date for %K indicator    
    date_for_K = rnames[row - i]
    
    sum = sum + calculate_K(stock_data, date_for_K, K_days)
  }
  
  # Calculate the exact average
  D = as.numeric(sum/MA_days)
  
  return (D)
}

#' Calculate slow %D
#'
#' @param stock_data stock data
#' @param date date at which to calculate
#' @param K_days number of days for %K indicator
#' @param MA_days number of days for moving average for %K
#' @param slow_MA_days number of days for moving average
#' 
#' @return value of slow %D
calculate_slow_D <- function(stock_data, date, K_days, MA_days, slow_MA_days) {
  # Dates from stock data
  rnames = row.names(as.data.frame(stock_data))
  
  # Get index for dates
  row = which(rnames == date)
  
  # Calculate Moving Average in K
  sum = 0
  for(i in (slow_MA_days - 1):0) {
    date_for_MA = rnames[row - i]
    
    sum = sum + calculate_D(stock_data, date_for_MA, K_days, MA_days)
  }
  
  # Calculate the exact average
  D = as.numeric(sum/MA_days)
  
  return (D)
}

#' Calculate momentum indicator
#'
#' @param stock_data stock data
#' @param date date at which to calculate
#' @param number_of_days number of days for momentum
#'
#' @return value of momentum
calculate_momentum <- function(stock_data, date, number_of_days) {
  # Start date
  rnames = row.names(as.data.frame(stock_data))
  
  today_date_index = which(rnames == date)
  today_close_price = stock_data[today_date_index, 4] # 4th column is close price
  
  previous_date_index = today_date_index - number_of_days
  previous_close_price = stock_data[previous_date_index, 4] # 4th column is close price
  
  momentum = as.numeric(today_close_price) - as.numeric(previous_close_price)
  
  return (momentum)
}

#' Calculate ROC indicator
#'
#' @param stock_data stock data
#' @param date date at which to calculate
#' @param number_of_days number of days to go back
#'
#' @return value of ROC indicator
calculate_ROC <- function(stock_data, date, number_of_days) {
  # Start date
  rnames = row.names(as.data.frame(stock_data))
  
  today_date_index = which(rnames == date)
  today_close_price = stock_data[today_date_index, 4] # 4th column is close price
  
  previous_date_index = today_date_index - number_of_days
  previous_close_price = stock_data[previous_date_index, 4] # 4th column is close price
  
  roc = (as.numeric(today_close_price) - as.numeric(previous_close_price)) / as.numeric(previous_close_price) * 100
  
  return (roc)
}

#' Calculate William's %R indicator
#'
#' @param stock_data stock data
#' @param date date at which to calculate
#' @param number_of_days number of days to go back
#'
#' @return value of William's %R indicator
calculate_williams_R <- function(stock_data, date, number_of_days) {
  # Start date
  rnames = row.names(as.data.frame(stock_data))
  start_date = rnames[which(rnames == date) - number_of_days]
  
  current_close = stock_data[end_date, 4] # 4th column is close price
  
  lowest_low = min(stock_data[str_c(cbind(start_date, date), collapse = "/"), 3]) # 3rd column is low price
  highest_high = max(stock_data[str_c(cbind(start_date, date), collapse = "/"), 2]) # 2nd column is high price
  
  r = (as.numeric(highest_high) - as.numeric(current_close)) / (as.numeric(highest_high) - as.numeric(lowest_low)) * 100
  
  return (r)
}

#' Calculate RSI indicator
#'
#' @param stock_data stock data
#' @param date date at which to calculate
#' @param number_of_days number of days to go back for returns
#'
#' @return value of RSI indicator
calculate_RSI <- function(stock_data, date, number_of_days) {
  # Start date
  rnames = row.names(as.data.frame(stock_data))
  today_date_index = which(rnames == date)
  previous_date_index = today_date_index - number_of_days
  
  avg_gains = 0
  gain_hits = 1
  
  avg_losses = 0
  loss_hits = 1
  
  for(day in (previous_date_index + 1):today_date_index) {
    return = as.numeric(stock_data[day, 4]) / as.numeric(stock_data[day - 1, 4]) - 1
    
    # Determine if it is gain or loss
    if(return > 0) {
      avg_gains = avg_gains + (return - avg_gains)/gain_hits
      gain_hits = gain_hits + 1
    } else {
      avg_losses = avg_losses + (return - avg_losses)/loss_hits
      loss_hits = loss_hits + 1
    }
  }
  
  rs = avg_gains / avg_losses

  rsi = 100 / (1 + rs)
  
  return (rsi)
}

#' Calculate A/D Oscillator indicator
#'
#' @param stock_data stock data
#' @param date date at which to calculate
#'
#' @return value of A/D Oscillator indicator
calculate_ad_oscillator <- function(stock_data, date) {
  # Start date
  rnames = row.names(as.data.frame(stock_data))
  today_date_index = which(rnames == date)
  previous_date_index = today_date_index - number_of_days
  
  current_high = stock_data[today_date_index, 2] # 2nd column is for high price
  current_low = stock_data[today_date_index, 3] # 3rd column is for low price
  previous_date_close = stock_data[previous_date_index, 4] # 4th column is for close price
  
  ad_oscillator = (as.numeric(current_high) - as.numeric(previous_date_close)) / (as.numeric(current_high) - as.numeric(current_low))
  
  return (ad_oscillator)
}

#' Calculate MA_days moving average
#'
#' @param stock_data stock data 
#' @param date date at which to calculate
#' @param MA_days days of moving average
#'
#' @return average close price for MA_days
calculate_moving_average <- function(stock_data, date, MA_days) {
  # Start date
  rnames = row.names(as.data.frame(stock_data))
  today_date_index = which(rnames == date)
  
  avg = as.numeric(stock_data[today_date_index, 4])
  for(day in 1:MA_days) {
    avg = avg  + (as.numeric(stock_data[today_date_index - day, 4]) - avg) / day
  }
  
  return (avg)
}

#' Calculate disparity indicator
#'
#' @param stock_data stock data
#' @param date date at which to calculate
#' @param MA_days days of moving average
#'
#' @return value of disparity
calculate_disparity <- function(stock_data, date, MA_days) {
  # Start date
  rnames = row.names(as.data.frame(stock_data))
  today_date_index = which(rnames == date)
  
  current_close = as.numeric(stock_data[today_date_index, 4]) # 4th column is for close price
  moving_average = calculate_moving_average(stock_data, date, MA_days)
  
  disparity = current_close / moving_average * 100
  
  return (disparity)
}

#' Calculate OSCP indicator
#'
#' @param stock_data stock data
#' @param date date at which to calculate
#' @param MA_days_1 days of the first moving average
#' @param MA_days_2 days of the second moving average
#'
#' @return value of OSCP indicator
calculate_oscp <- function(stock_data, date, MA_days_1, MA_days_2) {
  moving_average_1 = calculate_moving_average(stock_data, date, MA_days_1)
  moving_average_2 = calculate_moving_average(stock_data, date, MA_days_2)
  
  oscp = (moving_average_1 - moving_average_2) / moving_average_1
  
  return (oscp)
}

#' Calculate typical price
#'
#' @param stock_data stock data
#' @param date date at which to calculate
#'
#' @return value of the typical price
calculate_typical_price <- function(stock_data, date) {
  # Today's date
  rnames = row.names(as.data.frame(stock_data))
  today_date_index = which(rnames == date)
  
  typical_price = (as.numeric(stock_data[today_date_index, 2]) + as.numeric(stock_data[today_date_index, 3]) + as.numeric(stock_data[today_date_index, 4])) / 3 
  
  return (typical_price)
}

#' Calculate simple moving average for typical price
#'
#' @param stock_data stock data
#' @param date date at which to calculate
#' @param number_of_days time horizon for moving average
#'
#' @return value of the simple moving average for typical price
calculate_sma <- function(stock_data, date, number_of_days) {
  # Today's date
  rnames = row.names(as.data.frame(stock_data))
  today_date_index = which(rnames == date)
  
  sma = calculate_typical_price(stock_data, date)
  for(day in 1:number_of_days) {
    date_for_tp = rnames[today_date_index - day]
    sma = sma + (calculate_typical_price(stock_data, date_for_tp) - avg) / day
  }
  
  return (sma)
}

#' Calculate mean deviation
#'
#' @param stock_data stock data
#' @param date date at which to calculate
#' @param number_of_days time horizion
#'
#' @return value of mean deviation
calculate_mean_deviation <- function(stock_data, date, number_of_days) {
  # Today's date
  rnames = row.names(as.data.frame(stock_data))
  today_date_index = which(rnames == date)
  
  mean_deviation = abs(calculate_typical_price(stock_data, date) - calculate_sma(stock_data, date, number_of_days))
  for(day in 1:number_of_days) {
    date_for_tp = rnames[today_date_index - day]
    mean_deviation = mean_deviation + abs(calculate_typical_price(stock_data, date_for_tp) - avg) / day
  }
  
  return (mean_deviation)
}

#' Calculate CCI indicator
#'
#' @param stock_data stock data
#' @param date date at which to calculate
#' @param number_of_days time horizon
#'
#' @return value of CCI indicator
calculate_cci <- function(stock_data, date, number_of_days, const_var) {
  typical_price = calculate_typical_price(stock_data, date)
  sma = calculate_sma(stock_data, date, number_of_days)
  mean_deviation = calculate_mean_deviation(stock_data, date, number_of_days)
  
  cci = (typical_price - sma) / (const_var * mean_deviation) # 0.015 ???
  
  return (cci)
}