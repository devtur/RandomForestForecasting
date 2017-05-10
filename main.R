library(quantmod)
library(stringr)

source(indicators)

# Import data from Yahoo! Finance
stock = "DAX"
getSymbols(stock, src = "yahoo")

