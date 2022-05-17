# TSA

# S&P 500
# SPI


#install.packages("quantmod")

library(quantmod)
library(zoo)
library(xts)
library(tseries)

if(!require('YRmisc')) {
  install.packages('YRmisc')
  library('YRmisc')
}

# loading data from yahoo
data <- NULL
tickers <- c('^GSPC', 'AGG', 'BTC-USD')
for (Ticker in tickers){
  data <- cbind(data,
                      getSymbols.yahoo(Ticker, from="2014-09-15", end='2022-04', periodicity = "weekly", auto.assign=FALSE)[,6])
}

# removing NA
data <- na.omit(data)

colnames(data) <- c('S&P500', 'AGG', 'BTC-USD')

# plotting all three time series
par(mfrow = c(3,1))
plot(data$`S&P500`, main='S&P500')
plot(data$AGG, main='AGG')
plot(data$`BTC-USD`, main='BTC-USD')


# converting data into timeseries objects
data.ts <- ts(data, start=c(2014,9), frequency = 52)
plot(data.ts)

sp500.ts <- ts(data$`S&P500`, start=c(2014,9), frequency = 52)
agg.ts <- ts(data$AGG, start=c(2014,9), frequency = 52)
btc.ts <- ts(data$`BTC-USD`, start=c(2014,9), frequency = 52)

# decomposition of data
par(mfrow = c(1,1))
sp500.dec <- decompose(sp500.ts)
plot(sp500.dec)

agg.dec <- decompose(agg.ts)
plot(agg.dec)

btc.dec <- decompose(btc.ts)
plot(btc.dec)


# checking if time series is stationary. otherwhise use diff(log()) --> augmented dickey-fuller test
sp500.adf <- adf.test(sp500.ts)
sp500.adf$p.value # if p value > 0.05, we fail to reject nullhypothesis --> not stationary
sp500.adf <- adf.test(diff(log(sp500.ts)))
sp500.adf$p.value # with a log transformation and differentiation --> stationary

agg.adf <- adf.test(diff(log(agg.ts)))
agg.adf$p.value

btc.adf <- adf.test(diff(log(btc.ts)))
btc.adf$p.value

# differentiation and log transformation
plot(sp500.ts, main='S&P500')
plot(log(sp500.ts), main='Log transformed S&P500')
plot(diff(log(sp500.ts)), main='Differentiation of log transformed S&P500')

plot(agg.ts, main='AGG')
plot(log(agg.ts), main='Log transformed AGG')
plot(diff(log(agg.ts)), main='Differentiation of log transformed AGG')

plot(btc.ts, main='BTC-USD')
plot(log(btc.ts), main='Log transformed BTC-USD')
plot(diff(log(btc.ts)), main='Differentiation of log transformed BTC-USD')


# log returns for all three s&P500 = [,1], agg = [,2], btc-usd = [,3]
log_returns <- diff(log(data.ts))

# correlation between S&P500 and AGG
sp500.agg.cor <- cor(log_returns[,1], log_returns[,2])
sp500.agg.cor # no correlation --> 0.13

# correlation between S&P500 and BTC
sp500.btc.cor <- cor(log_returns[,1], log_returns[,3])
sp500.btc.cor # no correlation --> 0.16

# correlation between agg and BTC
agg.btc.cor <- cor(log_returns[,2], log_returns[,3])
agg.btc.cor # no correlation --> 0.044



# return correlations of crypto assets (e.g bitcoin, Ether) and traditional assets (e.g stocks, bonds) change over time and 
# whether they differ in rising and falling markets (of traditional assets) --> upside and downside of betas of a regression of crypto return on different traditional assets.
# beta https://www.rdocumentation.org/packages/YRmisc/versions/0.1.6/topics/pt.dbeta
# high beta implies higher risk with higher reward while low beta implies lower risk with lower reward (at least that is the theory).
# (e.g. a stock with a beta of 2.00 will gain twice as much as the market on up days and lose twice as much as the market on down days)

# pt.dbeta(ar,mr,rf) .. ar: a vector of a risk asset return, mr:a vector of market return, rf:risk free rate

# assuming market return is given by the S&P500
btc.sp500.betas <- pt.dbeta(log_returns[,3], log_returns[,1], 0)
btc.sp500.betas


# assuming market return is given by the AGG
btc.agg.betas <- pt.dbeta(log_returns[,3], log_returns[,2], 0)
btc.agg.betas


sp500.agg.betas <- pt.dbeta(log_returns[,1],log_returns[,2],0)
sp500.agg.betas$upbeta
sp500.agg.betas$downbeta

sp500.btc.betas <- pt.dbeta(log_returns[,1],log_returns[,3],0)
sp500.btc.betas$upbeta
sp500.btc.betas$downbeta

agg.btc.betas <- pt.dbeta(log_returns[,2],log_returns[,3],0)
agg.btc.betas$upbeta
agg.btc.betas$downbeta



counts <- c(btc.sp500.betas$upbeta, btc.sp500.betas$downbeta)
barplot(counts, main="Beta values",
        xlab="BTC", col=c("red","green"),
        legend = rownames(counts), beside=TRUE)




















