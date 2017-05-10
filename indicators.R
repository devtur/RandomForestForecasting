library(stringr)

#' Calculate stochastic %K indicator
#'
#' @param stock_data stock data 
#' @param start_date start of the time horizon
#' @param end_date end of the time horizon
#'
#' @return value of %K
calculate_K <- function(stock_data, start_date, end_date) {
  current_close = stock_data[end_date]$DAX.Close
  
  lowest_low = min(stock_data[str_c(cbind(start_date, end_date), collapse = "/")]$DAX.Low)
  highest_high = max(stock_data[str_c(cbind(start_date, end_date), collapse = "/")]$DAX.High)
  
  K = (current_close - lowest_low) / (highest_high - lowest_low) * 100
  
  K = as.numeric(K)
  
  return (K)
}

#' Calculate stochastic %D indicator
#'
#' @param stock_data 
#' @param date 
#' @param start_date_for_K 
#' @param MA_days 
#'
#' @return
#' @export
#'
#' @examples
calculate_D <- function(stock_data, date, start_date_for_K, MA_days) {
  # We needed basis to go backwards
  row = which(DAX == DAX[date]&Close)
  
  # Calculate Moving Average in K
  sum = 0
  for(i in (MA_days - 1):0) {
    date_for_MA = DAX[row - i, 0]
    
    sum = sum + calculate_K(start_date_for_K, date_for_MA)
  }
  
  D = as.numeric(sum/MA_days)
  
  return (D)
}