library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)
library(zoo)
library(dygraph)


stock.list<- c('AYX', 'GOOGL','FB')


# Technical analysis
getSymbols("PDD",from="2019-01-01",to="2019-11-21")
AYX_log_returns<-PDD%>%Ad()%>%dailyReturn(type='log')
PDD%>%Ad()%>%chartSeries()
PDD%>%chartSeries(TA='addBBands();addVo();addMACD()')

str(AYX)

x<-c('pose 1', 'pose 2', 'pose 3')
rand(x)


# stock correlation
library(PerformanceAnalytics)
getSymbols("GOOGL",from="2018-01-01",to="2019-11-21")
getSymbols("AMAZ",from="2018-01-01",to="2019-11-21")
getSymbols("FB",from="2018-01-01",to="2019-11-21")

# Amazon
AMZN_log_returns<-AMZN%>%Ad()%>%dailyReturn(type='log')
AMZN%>%Ad()%>%chartSeries()
AMZN%>%chartSeries(TA='addBBands();addVo();addMACD()',subset=c('2019','2018'))

# Google
GOOGL_log_returns<-GOOGL%>%Ad()%>%dailyReturn(type='log')
GOOGL%>%Ad()%>%chartSeries()
GOOGL%>%chartSeries(TA='addBBands();addVo();addMACD()',subset='2019')


data<-cbind(diff(log(Cl(AMZN))),diff(log(Cl(GOOGL))),diff(log(Cl(AAPL))),diff(log(Cl(FB))))
chart.Correlation(data)
dygraph(OHLC(AAPL))
