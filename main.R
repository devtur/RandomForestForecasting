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
  # Search for start indices in stock data
  rnames = row.names(as.data.frame(stock_data))
  start_date_index = which(rnames == start_date)
  end_date_index = which(rnames == end_date)
  
  # Matrix for data
  matrix_st = matrix(0, nrow = (end_date_index - start_date_index + 1), ncol = 13)
  
  # Parameters for indicators
  K_days = 14
  MA_days_for_D = 3
  MA_days_for_slow_D = 3
  number_of_days_momentum = 4
  number_of_days_ROC = 3
  number_of_days_R = 3
  number_of_days_RSI = 4
  MA_days_disparity_1 = 5
  MA_days_disparity_2 = 10
  number_of_days_CCI = 4
  const_var_CCI = 0.015
  
  for(day in start_date_index:end_date_index) {
    today_date = rnames[day]
    print(day)
    # Calculate indicators' values
    K = calculate_K(stock_data, today_date, K_days)                                                # 1. %K
    D = calculate_D(stock_data, today_date, K_days, MA_days_for_D)                                 # 2. %D
    slow_D = calculate_slow_D(stock_data, today_date, K_days, MA_days_for_D, MA_days_for_slow_D)   # 3. slow %D
    momentum = calculate_momentum(stock_data, today_date, number_of_days_momentum)                 # 4. Momentum
    ROC = calculate_ROC(stock_data, today_date, number_of_days_ROC)                                # 5. ROC
    williamsR = calculate_williams_R(stock_data, today_date, number_of_days_R)                     # 6. William's R
    RSI = calculate_RSI(stock_data, today_date, number_of_days_RSI)                                # 7. RSI
    ad_oscillator = calculate_ad_oscillator(stock_data, today_date)                                # 8. A/D Oscillator
    disparity_1 = calculate_disparity(stock_data, today_date, MA_days_disparity_1)                 # 9. Disparity
    disparity_2 = calculate_disparity(stock_data, today_date, MA_days_disparity_2)                 # 10. Disparity
    cci = calculate_cci(stock_data, today_date, number_of_days_CCI, const_var_CCI)                 # 11. CCI
    
    # Binary stock movement TRUE is for up, FALSE is for down
    stock_movement = FALSE
    return = as.numeric(stock_data[day + 1, 4]) / as.numeric(stock_data[day, 4])

    # Check whenever it is up or down movement
    if(return > 1) {
      stock_movement = TRUE
    }
    
    # Create new row for data frame
    new_entity = c(as.character(today_date), 
                   as.character(K), 
                   as.character(D), 
                   as.character(slow_D), 
                   as.character(momentum), 
                   as.character(ROC), 
                   as.character(williamsR), 
                   as.character(RSI), 
                   as.character(ad_oscillator), 
                   as.character(disparity_1), 
                   as.character(disparity_2), 
                   as.character(cci), 
                   as.character(stock_movement))
    
    # Merge
    matrix_st[day - start_date_index + 1, ] = new_entity
  }
  df_stock_data = as.data.frame(matrix_st)
  names(df_stock_data) = c("Date", 
                           "K", 
                           "D", 
                           "Slow_D", 
                           "Momentum", 
                           "ROC", 
                           "WilliamsR", 
                           "RSI", 
                           "AD_Oscillator", 
                           "Disparity_1", 
                           "Disparity_2", 
                           "CCI", 
                           "Stock_Movement")
  
  df_stock_data$Date = as.Date(as.character(df_stock_data$Date))
  df_stock_data$K = as.numeric(as.character(df_stock_data$K))
  df_stock_data$D = as.numeric(as.character(df_stock_data$D))
  df_stock_data$Slow_D = as.numeric(as.character(df_stock_data$Slow_D))
  df_stock_data$Momentum = as.numeric(as.character(df_stock_data$Momentum))
  df_stock_data$ROC = as.numeric(as.character(df_stock_data$ROC))
  df_stock_data$WilliamsR = as.numeric(as.character(df_stock_data$WilliamsR))
  df_stock_data$RSI = as.numeric(as.character(df_stock_data$RSI))
  df_stock_data$AD_Oscillator = as.numeric(as.character(df_stock_data$AD_Oscillator))
  df_stock_data$Disparity_1 = as.numeric(as.character(df_stock_data$Disparity_1))
  df_stock_data$Disparity_2 = as.numeric(as.character(df_stock_data$Disparity_2))
  df_stock_data$CCI = as.numeric(as.character(df_stock_data$CCI))
  df_stock_data$Stock_Movement = as.logical(as.character(df_stock_data$Stock_Movement))
  
  return (df_stock_data)
}

#' Run random forest classificaton
#'
#' @param train dataset to train
#'
#' @return classification summary
runRandomForest <- function(train, ntree = 300, mtry = 3) {
  out <- randomForest(as.factor(Stock_Movement) ~ K + D + Slow_D + Momentum + ROC + WilliamsR + RSI + Disparity_1 + Disparity_2 + CCI, data = train, ntree = ntree, mtry = mtry)

  return (out)
}

