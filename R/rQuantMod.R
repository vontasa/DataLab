
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)


# helper function to model Couch Potato strategy - a fixed allocation strategy
couch.potato.strategy <- function(
  data.all,
  tickers = 'XIC.TO,XSP.TO,XBB.TO',
  weights = c( 1/3, 1/3, 1/3 ),       
  periodicity = 'years',
  dates = '1900::',
  commission = 0.1){ 
  #*****************************************************************
  # Load historical data 
  #****************************************************************** 
  tickers = spl(tickers)
  names(weights) = tickers
  
  data <- new.env()
  for(s in tickers) data[[ s ]] = data.all[[ s ]]
  
  bt.prep(data, align='remove.na', dates=dates)
  
  #*****************************************************************
  # Code Strategies
  #******************************************************************
  prices = data$prices   
  n = ncol(prices)
  nperiods = nrow(prices)
  
  # find period ends
  period.ends = endpoints(data$prices, periodicity)
  period.ends = c(1, period.ends[period.ends > 0])
  
  #*****************************************************************
  # Code Strategies
  #******************************************************************
  data$weight[] = NA
  for(s in tickers) data$weight[period.ends, s] = weights[s]
  model = bt.run.share(data, clean.signal=F, commission=commission)
  return(model)
}


library(quantmod)
#*****************************************************************
# Load historical data
#****************************************************************** 
tickers = spl('VIPSX,VTSMX,VGTSX,SPY,TLT,GLD,SHY')

data <- new.env()
getSymbols(tickers, src = 'google', from = '1995-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)    

# extend GLD with Gold.PM - London Gold afternoon fixing prices
data$GLD = extend.GLD(data$GLD)

#*****************************************************************
# Code Strategies
#****************************************************************** 
models = list()
periodicity = 'years'
dates = '2003::'

models$classic = couch.potato.strategy(data, 'VIPSX,VTSMX', rep(1/2,2), periodicity, dates)
models$margarita = couch.potato.strategy(data, 'VIPSX,VTSMX,VGTSX', rep(1/3,3), periodicity, dates)
models$permanent = couch.potato.strategy(data, 'SPY,TLT,GLD,SHY', rep(1/4,4), periodicity, dates)

#*****************************************************************
# Create Report
#****************************************************************** 
plotbt.custom.report.part1(models)

