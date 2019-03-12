library(quantmod)
library(xts)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)
library(zoo)
library(tibble)
#--------Function---------

get_stock_prices <- function(ticker, return_format = "tibble", ...) {
  # Get stock prices
  stock_prices_xts <- getSymbols(Symbols = ticker, auto.assign = FALSE, ...)
  # Rename
  names(stock_prices_xts) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  # Return in xts format if tibble is not specified
  if (return_format == "tibble") {
    stock_prices <- stock_prices_xts %>%
      as_tibble() %>%
      rownames_to_column(var = "Date") %>%
      mutate(Date = ymd(Date))
  } else if(return_format == 'dataframe'){
    stock_prices <- stock_prices_xts %>%
      as.data.frame() %>%
      rownames_to_column(var = "Date") %>%
      mutate(Date = ymd(Date))
  }
  else{
    stock_prices <- stock_prices_xts
  }
  stock_prices
}

"MA" %>% get_stock_prices(return_format = 'dataframe')
