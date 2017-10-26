rm(list = ls())
da <- read.csv("C:/Users/Administrator/Desktop/zj/lesson 1 reference/GOOGL.csv")
loss <- -diff(log(da$Adj.Close))*100 # compute loss variable

###Empirical Quantile
VaR1 <- quantile(loss,0.95)
sloss <- sort(loss, decreasing = FALSE)
ES1 <- sum(sloss[length(sloss):round(0.95*length(sloss))])/(length(sloss)-round(0.95*length(sloss)))#0.95*1258=1194.15

###GARCH-std simulation
library(rugarch)
spec3 <- ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=TRUE),variance.model = list(garchOrder=c(1,1)), distribution.model = 'std')
fit3 <- ugarchfit(data = loss,spec = spec3) 
sig =sigma(fit3)

show(fit3)
mu=-0.068012
alpha = c(0.008991,0.010173)
beta=0.984053
df = 4.015649

library(fGarch)
t = 5
nround = 30000
set.seed(42)
err = matrix(rstd(t*nround,mean=0,sd=1,nu=df),t,nround)

ini=c(loss[1257],sig[1257])
xt=NULL
for (j in 1:nround){
  lt=NULL
  at=ini[1]-mu
  vart=ini[2]^2
  for(i in 1:t){
    var = alpha[1]+alpha[2]*at[i]^2+beta*vart[i]
    vart = c(vart,var)
    at = c(at,sqrt(var)*err[i,j])
    lt = c(lt,at[i+1]+mu)
  }
  xt = c(xt,sum(lt))
}

VaR2 <- quantile(xt,c(0.95)) # = 3.469699
idx=c(1:30000)[xt > VaR2]
mean(xt[idx]) #multi-period ES

"Firstwins" <- function(n = 10, nter = 1000){
  a <- 1:nter
  score1 <- 0
  score2 <- 0
  a[1] <- sample.int(10, size = 1, replace = TRUE)
  for(i in 2:nter){
    a[i] <- sample.int(n, size = 1, replace = TRUE)
    score1 <- score1 + (a[i-1] >= a[i])
    score2 <- score2 + (a[i-1] < a[i])
  }
}