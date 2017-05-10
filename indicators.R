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
  
  momentum = today_close_price - previous_close_price
  
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
  
  roc = (today_close_price - previous_close_price)/previous_close_price * 100
  
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
  
  r = (highest_high - current_close) / (highest_high - lowest_low) * 100
  
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
  start_date_index = today_date_index - number_of_days
  
  avg_gains = 0
  gain_hits = 1
  
  avg_losses = 0
  loss_hits = 1
  for(day in (start_date_index + 1):today_date_index) {
    return = stock_data[day, 4] / stock_data[day - 1, 4] - 1
    
    # Determine if it is gain or loss
    if(return > 0) {
      avg_gains = avg_gain + (return - avg_gain)/gain_hits
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

