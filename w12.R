rm(list = ls())
da = read.csv("C:/Users/Administrator/Desktop/zj/lesson 1 reference/GOOGL.csv")
loss = -diff(log(da$Adj.Close))*100 # compute loss variable
library(rugarch)
y = loss - mean(loss)
acf(y)
Box.test(y^2, lag = 28, type = 'Ljung-Box')
#GARCH
spec1 <- ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=TRUE),variance.model = list(garchOrder=c(1,1)))
fit1 = ugarchfit(data=loss,spec=spec1) 
show(fit1)
forecast1 <- ugarchforecast(fit1, n.ahead = 5) 
sigmahat <- sigma(forecast1)  # 5-step ahead forecast of the volatility of loss variable
wvarfore <- sum(sigmahat^2)
VaR095 <- qnorm(0.95)*sqrt(wvarfore)
ES095 <- dnorm(qnorm(0.95))*sqrt(wvarfore)/0.05
print(cbind(VaR095, ES095))

#RiskMetrics
spec2 <- ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=FALSE), variance.model = list(model ='iGARCH', garchOrder=c(1,1)))
fit2 <- ugarchfit(data = loss,spec = spec2)
forecast2 <- ugarchforecast(fit2, n.ahead = 1) 
rsigmahat <- sigma(forecast2)
rVaR095 <- qnorm(0.95)*sqrt(5)*rsigmahat
rES095 <- dnorm(qnorm(0.95))*sqrt(5)*rsigmahat/0.05
print(cbind(rVaR095, rES095))
