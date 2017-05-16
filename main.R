library(quantmod)
library(stringr)
library(randomForest)
library(nnet)
library(scales)
library(Quandl)

source("indicators.R")

args = commandArgs(trailingOnly = TRUE)

#' Create data frame of the needed data
#'
#' @param stock_data stock data
#' @param start_date start date for analysis
#' @param end_date end date for analysis
#'
#' @return data frame with indicators
createData <- function(stock_data, start_date, end_date) {
  # Search for start indices in stock data
  #rnames = row.names(as.data.frame(stock_data))
  
  row.names(stock_data) <- stock_data[, 1]
  rnames = row.names(stock_data)
  stock_data = stock_data[, -1]
  
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
    #print(day)
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
  out <- randomForest(as.factor(Stock_Movement) ~ K 
                      + D 
                      + Slow_D 
                      + Momentum 
                      + ROC 
                      + WilliamsR 
                      + RSI 
                      + Disparity_1 
                      + Disparity_2 
                      + AD_Oscillator
                      + CCI,
                      data = train, ntree = ntree, mtry = mtry)

  return (out)
}

#' Run neural net on data
#'
#' @param train dataset
#'
#' @return neural net summary
runNeuralNet <- function(train, threshold = 0.01, size = 50, iterations = 10000) {
  out <- nnet(as.factor(Stock_Movement) ~ K
                      + D
                      + Slow_D
                      + Momentum
                      + ROC
                      + WilliamsR
                      + RSI
                      + Disparity_1
                      + Disparity_2
                      + AD_Oscillator
                      + CCI,
                      data = train, size = size, threshold = threshold, maxit = iterations, trace = FALSE)
  
  return (out)
}

#' Data standartization to [0, 1]
#'
#' @param data dataset
#'
#' @return normalized dataset
normalizeData <- function(data) {
  n = ncol(data)
  
  # 2 because 1st is date column
  for(i in 2:(n-1)) {
    data[, i] = (data[, i] - min(data[, i])) / (max(data[, i]) - min(data[, i]))
  }
  
  return (data)
}

#' Test model on data
#'
#' @param model Model
#' @param test dataset to test
#'
#' @return percentage of wrong predictions
testModel <- function(model, test) {
  table = table(test[, 13], predict(model, test[, -13], type = "class"))
  
  error = (table[1, 2] + table[2, 1]) / sum(table)
  
  return (percent(error))
}

#' Run the whole project
#'
#' @return print report
runEverything <- function() {
  
  # Print output to report.txt
  sink("report.txt")
  
  write("*******************************************", file = "log.txt", sep = "\n", append = TRUE)
  print("*******************************************")
  print(date())
  write(date(), file = "log.txt", sep = "\n", append = TRUE)
  
  print("Obtaining data for DAX index...")
  write("Obtaining data for DAX index...", file = "log.txt", sep = "\n", append = TRUE)
  # Import data from Yahoo! Finance
  stock_data = Quandl("YAHOO/INDEX_GDAXI")
  print("Data is ready.")
  write("Data is ready.", file = "log.txt", sep = "\n", append = TRUE)
  
  # Exclude non-trading days
  #stock_data = stock_data[which(stock_data$Volume != 0), ]
  stock_data = stock_data[which(stock_data$High != stock_data$Low), ]
  
  # Revert in a way to put old days on top
  stock_data = stock_data[nrow(stock_data):1, ]
  
  start_date_train = "1990-12-28"
  end_date_train = "2011-04-13"
  #start_date_train = "2000-08-18"
  #end_date_train = "2014-04-28"
  
  start_date_test = "2011-04-14"
  end_date_test = "2017-05-11"
  # start_date_test = "2014-04-29"
  # end_date_test = "2017-05-11"
  
  print(sprintf("Training period from %s to %s.", start_date_train, end_date_train))
  write(sprintf("Training period from %s to %s.", start_date_train, end_date_train), file = "log.txt", sep = "\n", append = TRUE)
  print(sprintf("Testing period from %s to %s.", start_date_test, end_date_test))
  write(sprintf("Testing period from %s to %s.", start_date_test, end_date_test), file = "log.txt", sep = "\n", append = TRUE)
  
  print("-------------------")
  write("-------------------", file = "log.txt", sep = "\n", append = TRUE)
  print("Creating indicators...")
  write("Creating indicators...", file = "log.txt", sep = "\n", append = TRUE)
  train_data = createData(stock_data, start_date_train, end_date_train)
  test_data = createData(stock_data, start_date_test, end_date_test)
  print("Indicators created.")
  write("Obtaining data for DAX index...", file = "log.txt", sep = "\n", append = TRUE)
  
  print("-------------------")
  write("-------------------", file = "log.txt", sep = "\n", append = TRUE)
  print("Normalizing data...")
  write("Normalizing data...", file = "log.txt", sep = "\n", append = TRUE)
  train_data_normalized = normalizeData(train_data)
  test_data_normalized = normalizeData(test_data)
  print("Data is normalized.")
  write("Data is normalized.", file = "log.txt", sep = "\n", append = TRUE)
  
  print("-------------------")
  write("-------------------", file = "log.txt", sep = "\n", append = TRUE)
  
  mtry = as.numeric(args[1])
  number_of_trees = as.numeric(args[2])
  
  # mtry = 3
  # number_of_trees = 40
  
  print(sprintf("Running Random Forest model with %s trees and %s nodes to split...", number_of_trees, mtry))
  write(sprintf("Running Random Forest model with %s trees and %s nodes to split...", number_of_trees, mtry), file = "log.txt", sep = "\n", append = TRUE)
  out_random_forest <- runRandomForest(train_data_normalized, ntree = number_of_trees, mtry = mtry)
  print("Random Forest is built.")
  write("Random Forest is built.", file = "log.txt", sep = "\n", append = TRUE)
  
  
  print("-------------------")
  write("-------------------", file = "log.txt", sep = "\n", append = TRUE)
  
  size = as.numeric(args[3])
  iterations = as.numeric(args[4])
  
  # size = 25
  # iterations = 500
  
  print(sprintf("Running Neural Network model with %s hidden nodes and %s iterations...", size, iterations))
  write(sprintf("Running Neural Network model with %s hidden nodes and %s iterations...", size, iterations), file = "log.txt", sep = "\n", append = TRUE)
  out_neural_net <- runNeuralNet(train_data_normalized, size = size, iterations = iterations)
  print("Neural Net is built.")
  write("Neural Net is built.", file = "log.txt", sep = "\n", append = TRUE)
  
  print("-------------------")
  write("-------------------", file = "log.txt", sep = "\n", append = TRUE)
  print("Evaluating test sample...")
  write("Evaluating test sample...", file = "log.txt", sep = "\n", append = TRUE)
  error_random_forest = testModel(out_random_forest, test_data_normalized)
  error_neural_network = testModel(out_neural_net, test_data_normalized)
  
  print(sprintf("Random Forest got wrong %s of test sample.", error_random_forest))
  write(sprintf("Random Forest got wrong %s of test sample.", error_random_forest), file = "log.txt", sep = "\n", append = TRUE)
  print(sprintf("Neural Network got wrong %s of test sample.", error_neural_network))
  write(sprintf("Neural Network got wrong %s of test sample.", error_neural_network), file = "log.txt", sep = "\n", append = TRUE)
  write("*******************************************", file = "log.txt", sep = "\n", append = TRUE)
  print("*******************************************")
  
  return (0)
}

runEverything()
