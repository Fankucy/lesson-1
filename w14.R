rm(list = ls())
da <- read.csv("C:/Users/Administrator/Desktop/zj/lesson 1 reference/GOOGL.csv")
loss <- -diff(log(da$Adj.Close))*100 # compute loss variable

###Quantile Regression
library(quantreg)
spec1 = ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=TRUE),variance.model = list(garchOrder=c(1,1)))
fit1 = ugarchfit(data=loss,spec=spec1) 
sigl = sigma(fit1)
absl = abs(loss)
write.table(sigl, "sigl.csv",sep=",")
write.table(loss, "loss.csv",sep=",")
write.table(absl, "absl.csv",sep=",")
dd=read.table("C:/Users/Administrator/Desktop/zj/lesson 1 reference/GOOGL-rq.txt",header = TRUE)
mm = rq(loss~sig+absl,tau=0.95,data=loss)
summary(mm)

### class version
library(quantreg)
dd=read.csv("C:/Users/Administrator/Desktop/zj/lesson 1 reference/GOOGL-rq.csv",header = TRUE)
mm = rq(loss~sigl+absl,tau=0.95,data=dd)
summary(mm)
