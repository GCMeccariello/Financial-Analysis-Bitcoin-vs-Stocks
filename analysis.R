# Time Series Analysis in Finance
#
# Analysis of return correlation dynamics between traditional and crypto assets during boom-and-bust cycles
#
# 27.05.2022
# Giancarlo Meccariello


library('YRmisc')
library(quantmod)
library(zoo)
library(xts)
library(tseries)
library(moments)
library("Hmisc")
library('YRmisc')


# ============================================================== Load data
# loading data from yahoo
data <- NULL
tickers <- c('^GSPC', 'AGG', 'GC=F','BTC-USD','ETH-USD')
for (Ticker in tickers){
  data <- cbind(data,
                      getSymbols.yahoo(Ticker, from="2018-01-01", end='2022-04', periodicity = "daily", auto.assign=FALSE)[,6]) #start 2017 or 2014?
}

# removing NA
data <- na.omit(data)

colnames(data) <- c('S&P500', 'AGG', 'Gold','BTC-USD', 'ETH-USD')

# ============================================================== plot time series
# plotting all five time series
par(mfrow = c(5,1))
plot(data$`S&P500`, main='S&P500', ylab='Price in [USD]')
plot(data$AGG, main='AGG', ylab='Price in [USD]')
plot(data$Gold, main='Gold', ylab='Price in [USD]')
plot(data$`BTC-USD`, main='BTC-USD', ylab='Price in [USD]')
plot(data$`ETH-USD`, main='ETH-USD', ylab='Price in [USD]')


# plot of log of all four assets
data.log <- log(data)  
par(mfrow = c(1,1))
plot(data.log)
legend(x = "topright",
       col = c("black", "red", 'green', 'blue'), lty = 1, lwd = 1,
       legend = c('S&P500', 'AGG', 'BTC', 'ETH')) # legend does not work yet


# converting data into timeseries objects
data.ts <- ts(data, start=c(2018,1), end=c(2022,5), frequency = 365)
plot(data.ts) # plot all


l= c(1:5)
par(mfrow = c(5,1))
for (i in l) {
  plot(data.ts[,i], 
       main = colnames(data)[i],
       ylab = "Price in [USD]")
}


# ============================================================== Decomposition
# decomposition of data
par(mfrow = c(1,1))
sp500.dec <- decompose(data.ts[,1])
plot(sp500.dec)

agg.dec <- decompose(data.ts[,2])
plot(agg.dec)

gold.dec <- decompose(data.ts[,3])
plot(gold.dec)

btc.dec <- decompose(data.ts[,4])
plot(btc.dec)

eth.dec <- decompose(data.ts[,5])
plot(eth.dec)


# function to change the title of the decomposition plot
my_plot.decomposed.ts = function(x, title="", ...) {
  xx <- x$x
  if (is.null(xx)) 
    xx <- with(x, if (type == "additive") 
      random + trend + seasonal
      else random * trend * seasonal)
  par(mfrow=c(2,1))
  plot(cbind(observed = xx, trend = x$trend, seasonal = x$seasonal, random = x$random), 
       main=title, ...)
}

my_plot.decomposed.ts(sp500.dec, "Decomposition of S&P500")
my_plot.decomposed.ts(btc.dec, "Decomposition of Bitcoin")
my_plot.decomposed.ts(agg.dec, "Decomposition of AGG")
my_plot.decomposed.ts(gold.dec, "Decomposition of Gold")
my_plot.decomposed.ts(eth.dec, "Decomposition of Etherium")




# ============================================================== Stationarity
# checking if time series is stationary. otherwhise use diff(log()) --> augmented dickey-fuller test
sp500.adf <- adf.test(data.ts[,1])
sp500.adf # if p value > 0.05, we fail to reject nullhypothesis --> not stationary
sp500.adf <- adf.test(diff(log(data.ts[,1])))
sp500.adf$p.value # with a log transformation and differentiation --> stationary

agg.adf <- adf.test(diff(log(data.ts[,2])))
agg.adf$p.value

gold.adf <- adf.test(diff(log(data.ts[,3])))
gold.adf$p.value

btc.adf <- adf.test(diff(log(data.ts[,4])))
btc.adf$p.value

eth.adf <- adf.test(diff(log(data.ts[,5])))
eth.adf$p.value

# differentiation and log transformation
par(mfrow=c(3,1))
plot(data.ts[,1], main='S&P500', ylab='')
legend("topleft", legend = sprintf("P-Value: %s ", round(adf.test(data.ts[,1])$p.value,3)))
plot(log(data.ts[,1]), main='Log transformed S&P500', ylab='')
legend("topleft", legend = sprintf("P-Value: %s ", round(adf.test(log(data.ts[,1]))$p.value,3)))
plot(diff(log(data.ts[,1])), main='Differentiation of log transformed S&P500', ylab='')
legend("topleft", legend = sprintf("P-Value: %s ", round(adf.test(diff(log(data.ts[,1])))$p.value,3)))

par(mfrow=c(3,1))
plot(data.ts[,2], main='AGG', ylab='')
legend("topleft", legend = sprintf("P-Value: %s ", round(adf.test(data.ts[,2])$p.value,3)))
plot(log(data.ts[,2]), main='Log transformed AGG', ylab='')
legend("topleft", legend = sprintf("P-Value: %s ", round(adf.test(log(data.ts[,2]))$p.value,3)))
plot(diff(log(data.ts[,2])), main='Differentiation of log transformed AGG', ylab='')
legend("topleft", legend = sprintf("P-Value: %s ", round(adf.test(diff(log(data.ts[,2])))$p.value,3)))

par(mfrow=c(3,1))
plot(data.ts[,3], main='Gold', ylab='')
legend("topleft", legend = sprintf("P-Value: %s ", round(adf.test(data.ts[,3])$p.value,3)))
plot(log(data.ts[,3]), main='Log transformed Gold', ylab='')
legend("topleft", legend = sprintf("P-Value: %s ", round(adf.test(log(data.ts[,3]))$p.value,3)))
plot(diff(log(data.ts[,3])), main='Differentiation of log transformed Gold', ylab='')
legend("topleft", legend = sprintf("P-Value: %s ", round(adf.test(diff(log(data.ts[,3])))$p.value,3)))

par(mfrow=c(3,1))
plot(data.ts[,4], main='BTC-USD', ylab='')
legend("topleft", legend = sprintf("P-Value: %s ", round(adf.test(data.ts[,4])$p.value,3)))
plot(log(data.ts[,4]), main='Log transformed BTC-USD', ylab='')
legend("topleft", legend = sprintf("P-Value: %s ", round(adf.test(log(data.ts[,4]))$p.value,3)))
plot(diff(log(data.ts[,4])), main='Differentiation of log transformed BTC-USD', ylab='')
legend("topleft", legend = sprintf("P-Value: %s ", round(adf.test(diff(log(data.ts[,4])))$p.value,3)))

par(mfrow=c(3,1))
plot(data.ts[,5], main='ETH-USD', ylab='')
legend("topleft", legend = sprintf("P-Value: %s ", round(adf.test(data.ts[,5])$p.value,3)))
plot(log(data.ts[,5]), main='Log transformed ETH-USD', ylab='')
legend("topleft", legend = sprintf("P-Value: %s ", round(adf.test(log(data.ts[,5]))$p.value,3)))
plot(diff(log(data.ts[,5])), main='Differentiation of log transformed ETH-USD', ylab='')
legend("topleft", legend = sprintf("P-Value: %s ", round(adf.test(diff(log(data.ts[,5])))$p.value,3)))


# ============================================================== Statistics of entire timeline

# log returns for all three S&P500 = [,1], agg = [,2], gold = [,3], btc = [,4], eth = [,5]
log_returns <- diff(log(data.ts))

# descriptive statistics
mean.returns <- colMeans(log_returns)
mean.returns

# Calculate Standard Deviation
standev.returns <- round(sapply(log_returns, sd, na.rm = TRUE),3)
standev.returns

# calculate max
max.return <- round(sapply(log_returns, max, na.rm = TRUE),3)
max.return

# calculate min
min.return <- round(sapply(log_returns, min, na.rm = TRUE),3)
min.return

# calculate skewness
skew.return <- round(sapply(log_returns, skewness, na.rm = TRUE),3)
skew.return

# calculate kurtosis
kurt.return <- round(sapply(log_returns, kurtosis, na.rm = TRUE),3)
kurt.return

# construct a correlation matrix
cor.return <- cor(log_returns)
cor.return

# construct a covariance matrix
cov.return <- cov(log_returns)
cov.return

# ============================================================== Correlation and betas entire timeline
# correlation for entire time span
round(cor(log_returns),3)

# pt.dbeta(ar,mr,rf) .. ar: a vector of a risk asset return, mr:a vector of market return, rf:risk free rate

# assuming market return is given by the S&P500
btc.sp500.betas <- round(pt.dbeta(log_returns[,4], log_returns[,1], 0),3) 
btc.sp500.betas

eth.sp500.betas <- round(pt.dbeta(log_returns[,5], log_returns[,1], 0) ,3)
eth.sp500.betas

# market as Bond
btc.agg.betas <- round(pt.dbeta(log_returns[,4], log_returns[,2], 0) ,3)
btc.agg.betas

eth.agg.betas <- round(pt.dbeta(log_returns[,5], log_returns[,2], 0) ,3)
eth.agg.betas

# market as Gold
btc.gold.betas <- round(pt.dbeta(log_returns[,4], log_returns[,3], 0) ,3)
btc.gold.betas

eth.gold.betas <-round( pt.dbeta(log_returns[,5], log_returns[,3], 0) ,3)
eth.gold.betas


# ============================================================== Correlation for UP trend

# UPSIDE for two individual timeframes.
# uptrend:
# 26.12.18 - 12.02.20
# 247:531
# 
# 24.03.20 - 31.12.21
# 559:1007

log_return_1 <- diff(log(data[247:531,]))
log_return_1 <- na.omit(log_return_1)
round(cor(log_return_1),3)

log_return_2 <- diff(log(data[559:1007,]))
log_return_2 <- na.omit(log_return_2)
round(cor(log_return_2),3)

# ============================================================== Correlation for Down trend

# DOWN trend:
# 20.02.20 - 23.03.20
# 536:558
# 
# 28.03.22 - 20.05.22
# 1066:1104

log_return_3 <- diff(log(data[536:558,]))
log_return_3 <- na.omit(log_return_3)
round(cor(log_return_3),3)

log_return_4 <- diff(log(data[1066:1104,]))
log_return_4 <- na.omit(log_return_4)
round(cor(log_return_4),3)

# ============================================================== Rolling correlation
# rolling correlation BITCOIN and S&P500
cor_btc_sp500 <- NULL
for (a in c(1:(length(data[,1]) - 60))) {
  cor_btc_sp500 <-  cbind(cor_btc_sp500, cor(data[a:(a+59),4], data[a:(a+59),1]))
}
cor_btc_sp500 <- t(cor_btc_sp500)
colnames(cor_btc_sp500) <- c('Corr_btc_s&p')

cor_btc_sp500.ts <- ts(cor_btc_sp500, start=c(2018,3,18), end=c(2022,05,20),frequency=365)

par(mfrow=c(3,1), mai = c(0.3, 0.6, 0.2, 0.6) )
plot(data.ts[,1], ylab='Price in [USD]', main='S&P500')
plot(data.ts[,4], ylab='Price in [USD]', main='Bitcoin')
plot(cor_btc_sp500.ts, ylim=c(-1,1),ylab='Correlation [-]', main='60 Days Rolling Correlation')
abline(h=mean(cor_btc_sp500), col='red')
text(round(mean(cor_btc_sp500),3), x=(c(2018)), y = mean(cor_btc_sp500), pos = 1, col='red')
legend("topleft", legend = "Average Correlation", pch = "-", col = "red")


# rolling correlation ETHERIUM and S&P500
cor_eth_sp500 <- NULL
for (a in c(1:(length(data[,1]) - 60))) {
  cor_eth_sp500 <-  cbind(cor_eth_sp500, cor(data[a:(a+59),5], data[a:(a+59),1]))
}
cor_eth_sp500 <- t(cor_eth_sp500)
colnames(cor_eth_sp500) <- c('Corr_eth_s&p')

cor_eth_sp500 <- ts(cor_eth_sp500, start=c(2018,3,18), end=c(2022,05,20),frequency=365)

par(mfrow=c(3,1), mai = c(0.3, 0.6, 0.2, 0.6) )
plot(data.ts[,1], ylab='Price in [USD]', main='S&P500')
plot(data.ts[,5], ylab='Price in [USD]', main='Etherium')
plot(cor_eth_sp500, ylim=c(-1,1),ylab='Correlation [-]', main='60 Days Rolling Correlation')
abline(h=mean(cor_eth_sp500), col='red')
text(round(mean(cor_eth_sp500),3), x=(c(2018)), y = mean(cor_eth_sp500), pos = 1, col='red')
legend("topleft", legend = "Average Correlation", pch = "-", col = "red")


# rolling correlation BITCOIN and AGG
cor_btc_agg <- NULL
for (a in c(1:(length(data[,1]) - 60))) {
  cor_btc_agg <-  cbind(cor_btc_agg, cor(data[a:(a+59),4], data[a:(a+59),2]))
}
cor_btc_agg <- t(cor_btc_agg)
colnames(cor_btc_agg) <- c('Corr_btc_agg')

cor_btc_agg <- ts(cor_btc_agg, start=c(2018,3,18), end=c(2022,05,20),frequency=365)

par(mfrow=c(3,1), mai = c(0.3, 0.6, 0.2, 0.6) )
plot(data.ts[,2], ylab='Price in [USD]', main='AGG')
plot(data.ts[,4], ylab='Price in [USD]', main='Bitcoin')
plot(cor_btc_agg, ylim=c(-1,1),ylab='Correlation [-]', main='60 Days Rolling Correlation')
abline(h=mean(cor_btc_agg), col='red')
text(round(mean(cor_btc_agg),3), x=(c(2018)), y = mean(cor_btc_agg), pos = 1, col='red')
legend("topleft", legend = "Average Correlation", pch = "-", col = "red")


# rolling correlation ETHERIUM and AGG
cor_eth_agg <- NULL
for (a in c(1:(length(data[,1]) - 60))) {
  cor_eth_agg <-  cbind(cor_eth_agg, cor(data[a:(a+59),5], data[a:(a+59),2]))
}
cor_eth_agg <- t(cor_eth_agg)
colnames(cor_eth_agg) <- c('Corr_eth_s&p')

cor_eth_agg <- ts(cor_eth_agg, start=c(2018,3,18), end=c(2022,05,20),frequency=365)

par(mfrow=c(3,1), mai = c(0.3, 0.6, 0.2, 0.6) )
plot(data.ts[,2], ylab='Price in [USD]', main='AGG')
plot(data.ts[,5], ylab='Price in [USD]', main='Etherium')
plot(cor_eth_agg, ylim=c(-1,1),ylab='Correlation [-]', main='60 Days Rolling Correlation')
abline(h=mean(cor_eth_agg), col='red')
text(round(mean(cor_eth_agg),3), x=(c(2018)), y = mean(cor_eth_agg), pos = 1, col='red')
legend("topleft", legend = "Average Correlation", pch = "-", col = "red")


# rolling correlation BITCOIN and GOLD
cor_btc_gold <- NULL
for (a in c(1:(length(data[,1]) - 60))) {
  cor_btc_gold <-  cbind(cor_btc_gold, cor(data[a:(a+59),4], data[a:(a+59),3]))
}
cor_btc_gold <- t(cor_btc_gold)
colnames(cor_btc_gold) <- c('Corr_btc_agg')

cor_btc_gold <- ts(cor_btc_gold, start=c(2018,3,18), end=c(2022,05,20),frequency=365)

par(mfrow=c(3,1), mai = c(0.3, 0.6, 0.2, 0.6) )
plot(data.ts[,3], ylab='Price in [USD]', main='Gold')
plot(data.ts[,4], ylab='Price in [USD]', main='Bitcoin')
plot(cor_btc_gold, ylim=c(-1,1),ylab='Correlation [-]', main='60 Days Rolling Correlation')
abline(h=mean(cor_btc_gold), col='red')
text(round(mean(cor_btc_gold),3), x=(c(2018)), y = mean(cor_btc_gold), pos = 3, col='red')
legend("topleft", legend = "Average Correlation", pch = "-", col = "red")


# rolling correlation ETHERIUM and GOLD
cor_eth_gold <- NULL
for (a in c(1:(length(data[,1]) - 60))) {
  cor_eth_gold <-  cbind(cor_eth_gold, cor(data[a:(a+59),5], data[a:(a+59),3]))
}
cor_eth_gold <- t(cor_eth_gold)
colnames(cor_eth_gold) <- c('Corr_eth_s&p')

cor_eth_gold <- ts(cor_eth_gold, start=c(2018,3,18), end=c(2022,05,20),frequency=365)

par(mfrow=c(3,1), mai = c(0.3, 0.6, 0.2, 0.6) )
plot(data.ts[,3], ylab='Price in [USD]', main='Gold')
plot(data.ts[,5], ylab='Price in [USD]', main='Etherium')
plot(cor_eth_gold, ylim=c(-1,1),ylab='Correlation [-]', main='60 Days Rolling Correlation')
abline(h=mean(cor_eth_gold), col='red')
text(round(mean(cor_eth_gold),3), x=(c(2018)), y = mean(cor_eth_gold), pos = 1, col='red')
legend("topleft", legend = "Average Correlation", pch = "-", col = "red")
