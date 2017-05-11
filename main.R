library(quantmod)
library(stringr)

source("indicators.R")

# Import data from Yahoo! Finance
stock = "DAX"
getSymbols(stock, src = "yahoo")

#' Create data frame of the needed data
#'
#' @param stock_data stock data
#' @param start_date start date for analysis
#' @param end_date end date for analysis
#'
#' @return data frame with indicators
createData <- function(stock_data, start_date, end_date) {
  # The structure of the data frame
  df_stock_data <- data.frame(Date = as.Date(character()),
                   K = double(), 
                   D = double(),
                   Slow_D = double(),
                   Momentum = double(),
                   ROC = double(),
                   WilliamsR = double(),
                   RSI = double(),
                   AD_Oscillator = double(),
                   Disparity_1 = double(),
                   Disparity_2 = double(),
                   CCI = double(),
                   Stock_Movement = integer(), 
                   stringsAsFactors = FALSE) 
  
  # Search for start indices in stock data
  rnames = row.names(as.data.frame(stock_data))
  start_date_index = which(rnames == start_date)
  end_date_index = which(rnames == end_date)
  
  # Parameters for indicators
  K_days = 14
  MA_days_for_D = 3
  number_of_days_momentum = 4
  number_of_days_ROC = 3
  number_of_days_R = 3
  number_of_days_RSI = 3
  MA_days_disparity_1 = 5
  MA_days_disparity_2 = 10
  number_of_days_CCI = 4
  
  for(day in start_date_index:end_date_index) {
    today_date = rnames[day]
    
    # Calculate indicators' values
    K = calculate_K(stock_data, today_date, K_days)                                 # 1. %K
    D = calculate_D(stock_data, today_date, K_days, MA_days_for_D)                  # 2.
    slow_D = calculate_slow_D(stock_data, today_date, K_days, MA_days_for_slow_D)   # 3.
    momentum = calculate_momentum(stock_data, today_date, number_of_days_momentum)  # 4.
    ROC = calculate_ROC(stock_data, today_date, number_of_days_ROC)                 # 5.
    williamsR = calculate_williams_R(stock_data, today_date, number_of_days_R)      # 6.
    RSI = calculate_RSI(stock_data, today_date, number_of_days_RSI)                 # 7.
    ad_oscillator = calculate_ad_oscillator(stock_data, today_date)                 # 8. 
    disparity_1 = calculate_disparity(stock_data, today_date, MA_days_disparity_1)  # 9.
    disparity_2 = calculate_disparity(stock_data, today_date, MA_days_disparity_2)  # 10.
    cci = calculate_cci(stock_data, today_date, number_of_days_CCI)                 # 11.
    
    # Binary stock movement 1 is for up, 0 is for down
    stock_movement = 0
    return = stock_data[day + 1] / stock_data[day] - 1
    
    # Check whenever it is up or down movement
    if(return > 0) {
      stock_movement = 1
    }
    
    # Create new row for data frame
    new_entity = c(K, D, slow_D, momentum, ROC, williamsR, RSI, ad_oscillator, disparity_1, disparity_2, cci, stock_movement)
    
    # Merger
    df_stock_data <- rbind(df_stock_data, new_entity)
  }
  
  return (df_stock_data)
}
