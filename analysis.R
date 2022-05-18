# TSA

# S&P 500
# SPI

#install.packages("quantmod")


if(!require('YRmisc')) {
  install.packages('YRmisc')
  library('YRmisc')
}
library(quantmod)
library(zoo)
library(xts)
library(tseries)

# loading data from yahoo
data <- NULL
tickers <- c('^GSPC', 'AGG', 'BTC-USD','ETH-USD')
for (Ticker in tickers){
  data <- cbind(data,
                      getSymbols.yahoo(Ticker, from="2017-11-06", end='2022-04', periodicity = "weekly", auto.assign=FALSE)[,6]) #start 2017 or 2014?
}


# removing NA
data <- na.omit(data)

colnames(data) <- c('S&P500', 'AGG', 'BTC-USD', 'ETH-USD')

data_for_beta <- data

# plotting all three time series
par(mfrow = c(4,1))
plot(data$`S&P500`, main='S&P500')
plot(data$AGG, main='AGG')
plot(data$`BTC-USD`, main='BTC-USD')
plot(data$`ETH-USD`, main='ETH-USD')


# plot of log of all four lines
data.log <- log(data) 
par(mfrow = c(1,1))
plot(data.log)
legend(x = "topright",
       col = c("black", "red", 'green', 'blue'), lty = 1, lwd = 1,
       legend = c('S&P500', 'AGG', 'BTC', 'ETH')) # legend does not work yet


# converting data into timeseries objects
data.ts <- ts(data, start=c(2014,9), frequency = 52)
plot(data.ts)

# __________________________________________________________________________________ does not work
## Plot first set of data and draw its axis
plot(data[,1:2], axes=FALSE, xlab="", ylab="", ylim=c(-1000,5000), 
     type="l",col="black", main="Mike's test data")
#axis(2,col="black",las=1)  ## las=1 makes horizontal labels
#mtext("Beta Gal Absorbance",side=2,line=2.5)
#box()

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(data.log[,3:4], pch=15,  xlab="", ylab="", 
     axes=FALSE, type="l", col="red")
## a little farther out (line=4) to make room for labels
mtext("Cell Density",side=4,col="red",line=4) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)

## Draw the time axis
axis(1,pretty(range(time),10))
mtext("Time (Hours)",side=1,col="black",line=2.5)  

## Add Legend
legend("topleft",legend=c("Beta Gal","Cell Density"),
       text.col=c("black","red"),pch=c(16,15),col=c("black","red"))

# __________________________________________________________________________________


sp500.ts <- ts(data$`S&P500`, start=c(2017,11), frequency = 52)
agg.ts <- ts(data$AGG, start=c(2017,11), frequency = 52)
btc.ts <- ts(data$`BTC-USD`, start=c(2017,11), frequency = 52)
eth.ts <- ts(data$`ETH-USD`, start=c(2017,11), frequency = 52)

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

plot(eth.ts, main='ETH-USD')
plot(log(eth.ts), main='Log transformed ETH-USD')
plot(diff(log(eth.ts)), main='Differentiation of log transformed ETH-USD')


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

# correlation between BTC and ETH
btc.eth.cor <- cor(log_returns[,3], log_returns[,4])
btc.eth.cor # --> 0.777

eth.sp500.cor <- cor(log_returns[,4], log_returns[,1])
eth.sp500.cor # --> 0.283 

eth.agg.cor <- cor(log_returns[,4], log_returns[,2])
eth.agg.cor # --> 0.087 


# do we need any autocorrelation function?
a <- acf(log_returns[,2])


# return correlations of crypto assets (e.g bitcoin, Ether) and traditional assets (e.g stocks, bonds) change over time and 
# whether they differ in rising and falling markets (of traditional assets) --> upside and downside of betas of a regression of crypto return on different traditional assets.
# beta https://www.rdocumentation.org/packages/YRmisc/versions/0.1.6/topics/pt.dbeta
# high beta implies higher risk with higher reward while low beta implies lower risk with lower reward (at least that is the theory).
# (e.g. a stock with a beta of 2.00 will gain twice as much as the market on up days and lose twice as much as the market on down days)

# pt.dbeta(ar,mr,rf) .. ar: a vector of a risk asset return, mr:a vector of market return, rf:risk free rate

# assuming market return is given by the S&P500
btc.sp500.betas <- pt.dbeta(log_returns[,3], log_returns[,1], 0)
btc.sp500.betas

eth.sp500.betas <- pt.dbeta(log_returns[,4], log_returns[,1], 0)
eth.sp500.betas

lm.a <-  lm(log_returns[,3] ~ log_returns[,1])
lm.a
plot(lm.a)

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



# upsides for three indivual timeframes.
# 01.01.2017 – 24.01.2018
# 24.12.2018 – 19.02.2020
# 20.03.2020 – 28.12.2021

log_return_1 <- diff(log(data_for_beta[1:13,]))
log_return_2 <- diff(log(data_for_beta[60:120,]))
log_return_3 <- diff(log(data_for_beta[125:217,]))

# bitcoin as risk asset return, and S&P500 as market
btc.sp500.betas.1 <- pt.dbeta(log_return_1[,3], log_return_1[,1], 0)
btc.sp500.betas.1
btc.sp500.betas.2 <- pt.dbeta(log_return_2[,3], log_return_2[,1], 0)
btc.sp500.betas.2
btc.sp500.betas.3 <- pt.dbeta(log_return_3[,3], log_return_3[,1], 0)
btc.sp500.betas.3

# Etherium as risk asset return, and S&P500 as market
etc.sp500.betas.1 <- pt.dbeta(log_return_1[,4], log_return_1[,1], 0)
etc.sp500.betas.1
etc.sp500.betas.2 <- pt.dbeta(log_return_2[,4], log_return_2[,1], 0)
etc.sp500.betas.2
etc.sp500.betas.3 <- pt.dbeta(log_return_3[,4], log_return_3[,1], 0)
etc.sp500.betas.3












