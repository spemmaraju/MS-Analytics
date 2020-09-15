install.packages("quantmod")
library(quantmod)




getSymbols("JPYUSD=X", from = "2010-1-01", to = "2020-7-31")
yen<-na.fill(`JPYUSD=X`[,"JPYUSD=X.Close"], "extend")
summary(yen)


getSymbols("EURUSD=X", from = "2010-1-01", to = "2020-7-31")
eur<-na.fill(`EURUSD=X`[,"EURUSD=X.Close"], "extend")
summary(eur)

plot(yen)
plot(eur)
par(mfrow=c(1,1))



getwd()


#2  Set your working directory to "ANLY 580/RScript". 
setwd("C:/Users/subha/Desktop/ANLY565/RScript")

#3  
library(readxl)
library(tseries)
library(forecast)
pce_unemp<-read_excel("PCE_UNRATE.xlsx", col_types = c("date", "numeric", "numeric"))



#4  
date<-pce_unemp$DATE
pce<-pce_unemp$PCEPI
unemp <- pce_unemp$UNRATE

pcets<-ts(pce, start=c(1959,1), freq=12)

plot(pcets, xlab="Date Range", ylab="PCE", main="Personal Consumption Expenditure")
acf(pcets)
pacf(pcets)
# No, the variable does not appear stationary, because the ACF is decaying very slowly implying that
# the autocorrelation is a function of time


pce.pre <- window(pcets, start = c(1959,1), end = c(2010,12), freq = 12)
pce.post <- window(pcets, start = c(2011,1), freq = 12)


infl<-diff(log(pcets))

plot(infl)
# Right away from inflation plot, we can see evidence of some seasonality
acf(infl)
pacf(infl)
#Inflation is non stationary

infl.pre <- window(infl, start = c(1959,2), end = c(2010,12), freq = 12)
infl.post <- window(infl, start = c(2011,1), freq = 12)

plot(infl.pre)
acf(infl.pre)



get.best.sarima <- function(x.ts, maxord = c(1,1,1,1,1,1))
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
    for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p,d,q),
                   seas = list(order = c(P,D,Q),
                               frequency(x.ts)), method = "CSS")
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
      if (fit.aic < best.aic)
      {
        best.aic <- fit.aic
        best.fit <- fit
        best.fit.coef <- fit$coef
        best.model <- c(p,d,q,P,D,Q)
      }
    }
  list(best.aic, best.fit, best.fit.coef, best.model)
}

infl.best.sarima<-get.best.sarima(infl.pre, maxord = c(2,2,2,2,2,2))
infl.best.sarima[[3]]
# It is SARIMA (1,0,2,0,0,0) 

infl.sarima.resid <- infl.best.sarima[[2]]$residuals

acf(infl.sarima.resid)
acf(infl.sarima.resid^2)



infl.resid.garch<-garchFit(~garch(1,1), data=infl.sarima.resid,include.mean = F, trace = F)
infl.resid.garch
summary(infl.resid.garch)
acf(infl.resid.garch@residuals)
acf(infl.resid.garch@residuals^2)
infl.fit <- garchFit(~ arma(1,2)+garch(1,1), data = infl.pre,include.mean=T)
summary(infl.fit)
infl.fit

acf(infl.fit@residuals)
acf(infl.fit@residuals^2)

infl.pred1<-predict(infl.best.sarima[[2]], n.ahead=108)

infl.pred<-predict(infl.fit, n.ahead=108)
infl.post

ts.plot(infl.post, infl.pred1$pred, lty = 1:2, col=c("red", "blue"))


infl.model<-auto.arima(infl.pre)
acf(infl.model$residuals)
acf(infl.model$residuals^2)

infl.pred<-predict(infl.model, n.ahead=108)
ts.plot(infl.pre, infl.pred$pred, lty = 1:2, col=c("red", "blue"))


unempts<-ts(unemp, start=c(1959,1), freq=12)
unemp.pre <- window(unempts, start = c(1959,2), end = c(2010,12), freq = 12)
unemp.post <- window(unempts, start = c(2011,1), freq = 12)
acf(unemp.pre)
pacf(unemp.pre)
unemp.model<-auto.arima(unemp.pre)


unemp.best.sarima<-get.best.sarima(unemp.pre, maxord = c(2,2,2,2,2,2))
unemp.best.sarima[[1]]
acf(unemp.best.sarima[[2]]$residuals)

unemp.pred<-predict(unemp.best.sarima[[2]], n.ahead=108)
ts.plot(unemp.pre, unemp.pred$pred, lty = 1:2, col=c("red", "blue"))
ts.plot(unemp.post, unemp.pred$pred, lty = 1:2, col=c("red", "blue"))



## Final Project final splits
# 1959-1 - 1980-12
# 1981-1 - 2010-12


infl.pre1 <- window(infl, start = c(1959,2), end = c(1979,12), freq = 12)
infl.post1 <- window(infl, start = c(1980,1), end = c(1980,12), freq = 12)
infl.pre2 <- window(infl, start = c(1981,1), end = c(2009,12), freq = 12)
infl.post2 <- window(infl, start = c(2010,1), end = c(2010,12), freq = 12)

unemp.pre1 <- window(unempts, start = c(1959,2), end = c(1979,12), freq = 12)
unemp.post1 <- window(unempts, start = c(1980,1), end = c(1980,12), freq = 12)
unemp.pre2 <- window(unempts, start = c(1981,1), end = c(2009,12), freq = 12)
unemp.post2 <- window(unempts, start = c(2010,1), end = c(2010,12), freq = 12)

infl.pre1.dec<-decompose(infl.pre1)
plot(infl.pre1.dec)

infl.pre1.model<-auto.arima(infl.pre1)
infl.pre1.model

infl1.pred <- predict(infl.pre1.model, n.ahead=12)

ts.plot(infl.post1, infl1.pred$pred, lty = 1:2, col=c("red", "blue"))

MAPE(infl.post1, infl1.pred$pred)

infl.pre2.model<-auto.arima(infl.pre2)
infl.pre2.model
# Too many MA variables. So we will limit to 2 using custom SARIMA function

infl.pre2.model<-get.best.sarima(infl.pre2, maxord = c(2,2,2,2,2,2))
infl.pre2.model[[3]]
# It is SARIMA (2,0,2,1,0,0) 

infl.pre2.model.resid <- infl.pre2.model[[2]]$residuals

acf(infl.pre2.model.resid)
pacf(infl.pre2.model.resid)

infl2.pred <- predict(infl.pre2.model[[2]], n.ahead=12)

ts.plot(infl.post2, infl2.pred$pred, lty = 1:2, col=c("red", "blue"))

MAPE(infl.post2, infl2.pred$pred)

unemp.pre1.model<-auto.arima(unemp.pre1)
unemp.pre1.model

unemp.pre1.model.resid <- unemp.pre1.model[[2]]$residuals
acf(unemp.pre1.model.resid)
pacf(unemp.pre1.model.resid)

unemp1.pred<-predict(unemp.pre1.model[[2]], n.ahead=12)
ts.plot(unemp.post1, unemp1.pred$pred, lty=1:2, col=c("red", "blue"))
MAPE(unemp.post1, unemp1.pred$pred)


unemp.pre2.model<-auto.arima(unemp.pre2)
unemp.pre2.model

unemp.pre2.model.resid <- unemp.pre2.model$residuals
acf(unemp.pre1.model.resid)
pacf(unemp.pre1.model.resid)

unemp2.pred<-predict(unemp.pre2.model, n.ahead=12)
ts.plot(unemp.post2, unemp2.pred$pred, lty=1:2, col=c("red", "blue"))
MAPE(unemp.post1, unemp1.pred$pred)

adf.test(unemp.pre1)
adf.test(infl.pre1)

dunemp.pre1<-diff(unemp.pre1)
dinfl.pre1<-diff(infl.pre1)

adf.test(dunemp.pre1)
adf.test(dinfl.pre1)

inf_unemp.var <- VAR(cbind(dinfl.pre1, dunemp.pre1), p = 3, type = "both")
coef(inf_unemp.var)

plot(dunemp.pre1, dinfl.pre1, pch = 4)
cor(dunemp.pre1, dinfl.pre1)

po.test(cbind(infl.pre1, unemp.pre1))


coint.test(infl.pre1,unemp.pre1)

X <- matrix(rnorm(200),100,2)



US.var <- VAR(cbind(dGNP, dM1), p = 2, type = "trend")
coef(US.var)
acf(resid(US.var)[, 1])
acf(resid(US.var)[, 2])
US.pred <- predict(US.var, n.ahead = 4)
US.pred
dGNP.pred <- ts(US.pred$fcst$dGNP[, 1], st = 1988, fr = 4)
dM1.pred <- ts(US.pred$fcst$dM1[, 1], st = 1988, fr = 4)
ts.plot(cbind(window(dGNP, start = 1981), dGNP.pred), lty = 1:2)
ts.plot(cbind(window(dM1, start = 1981), dM1.pred), lty = 1:2)




# Project Code
# Load all the libraries we need
library(MLmetrics)
library(vars)
library(aTSA)

# Set working directory
setwd("C:/Users/subha/Desktop/ANLY565/RScript")

# Load Inflation and unemployment data
infl_unemp<-read_excel("INFL_UNRATE.xlsx", col_types = c("date", "numeric", "numeric"))


date<-infl_unemp$DATE
infl<-infl_unemp$INFL
unemp <- infl_unemp$UNRATE

inflts<-ts(infl, start=c(1959,1), freq=12)
unempts<-ts(unemp, start=c(1959,1), freq=12)

# Plot the full datasets
par(mfrow=c(2,1))
plot(inflts, xlab="Date Range", ylab="CPI Inflation", main="Inflation Rate")
plot(unempts, xlab="Date Range", ylab="Unemployment", main="Unemployment Rate")

# Split the data into pre and post 1980 period
infl.pre1  <- window(inflts, start = c(1959,2), end = c(1979,12), freq = 12)
infl.post1 <- window(inflts, start = c(1980,1), end = c(1980,12), freq = 12)
infl.pre2  <- window(inflts, start = c(1981,1), end = c(2006,12), freq = 12)
infl.post2 <- window(inflts, start = c(2007,1), end = c(2007,12), freq = 12)


# Function to develop a SARIMA model with a max order of 2
get.best.sarima <- function(x.ts, maxord = c(1,1,1,1,1,1))
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
    for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p,d,q),
                   seas = list(order = c(P,D,Q),
                               frequency(x.ts)), method = "CSS")
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
      if (fit.aic < best.aic)
      {
        best.aic <- fit.aic
        best.fit <- fit
        best.fit.coef <- fit$coef
        best.model <- c(p,d,q,P,D,Q)
      }
    }
  list(best.aic, best.fit, best.fit.coef, best.model)
}



infl.pre1.dec<-decompose(infl.pre1)
plot(infl.pre1.dec)

acf(infl.pre1)
pacf(infl.pre1)

infl.pre1.model<-get.best.sarima(infl.pre1, maxord = c(2,2,2,2,2,2))
infl.pre1.model[[2]]
# It is SARIMA(0,0,1,1,0,2)

acf(infl.pre1.model[[2]]$residuals)
pacf(infl.pre1.model[[2]]$residuals)

infl1.pred <- predict(infl.pre1.model[[2]], n.ahead=12)

par(mfrow=c(1,1))

ts.plot(infl.post1, infl1.pred$pred, lty = 1:2, col=c("red", "blue"), xlab="Date Range", ylab="Inflation", main="Inflation Rate")

MAPE(infl.post1, infl1.pred$pred)

#Pre 2
infl.pre2.dec<-decompose(infl.pre2)
plot(infl.pre2.dec)

par(mfrow=c(2,1))
acf(infl.pre2)
pacf(infl.pre2)

infl.pre2.model<-get.best.sarima(infl.pre2, maxord = c(2,2,2,2,2,2))
infl.pre2.model[[2]]
# It is SARIMA(1,0,2,2,0,2)


acf(infl.pre2.model[[2]]$residuals)
pacf(infl.pre2.model[[2]]$residuals)


infl2.pred <- predict(infl.pre2.model[[2]], n.ahead=12)
par(mfrow=c(1,1))

ts.plot(infl.post2, infl2.pred$pred, lty = 1:2, col=c("red", "blue"), xlab="Date Range", ylab="Inflation", main="Inflation Rate")

MAPE(infl.post2, infl2.pred$pred)


unemp.pre1  <- window(unempts, start = c(1959,2), end = c(1979,12), freq = 12)
unemp.post1 <- window(unempts, start = c(1980,1), end = c(1980,12), freq = 12)
unemp.pre2  <- window(unempts, start = c(1981,1), end = c(2006,12), freq = 12)
unemp.post2 <- window(unempts, start = c(2007,1), end = c(2007,12), freq = 12)

unemp.pre1.dec<-decompose(unemp.pre1)
plot(unemp.pre1.dec)

par(mfrow=c(2,1))
acf(unemp.pre1)
pacf(unemp.pre1)

unemp.pre1.model<-get.best.sarima(unemp.pre1)
unemp.pre1.model[[2]]
# It is SARIMA(1,0,1,1,0,0)

acf(unemp.pre1.model[[2]]$residuals)
pacf(unemp.pre1.model[[2]]$residuals)

par(mfrow=c(1,1))
unemp1.pred<-predict(unemp.pre1.model[[2]], n.ahead=12)

ts.plot(unemp.post1, unemp1.pred$pred, lty=1:2, col=c("red", "blue"), xlab="Date Range", ylab="Unemployment", main="Unemployment Rate")
MAPE(unemp.post1, unemp1.pred$pred)

par(mfrow=c(2,1))
acf(unemp.pre2)
pacf(unemp.pre2)

unemp.pre2.model<-get.best.sarima(unemp.pre2)
unemp.pre2.model[[2]]
# It is SARIMA (1,0,1,1,0,0)

par(mfrow=c(2,1))
acf(unemp.pre2.model[[2]]$residuals)
pacf(unemp.pre2.model[[2]]$residuals)

par(mfrow=c(1,1))
unemp2.pred<-predict(unemp.pre2.model[[2]], n.ahead=12)
ts.plot(unemp.post2, unemp2.pred$pred, lty=1:2, col=c("red", "blue"), xlab="Date Range", ylab="Unemployment", main="Unemployment Rate")
MAPE(unemp.post2, unemp2.pred$pred)

adf.test(unemp.pre1)
adf.test(infl.pre1)

dunemp.pre1<-diff(unemp.pre1)
dinfl.pre1<-diff(infl.pre1)

adf.test(dunemp.pre1)
adf.test(dinfl.pre1)

po.test(cbind(infl.pre1, unemp.pre1))

inf_unemp.var <- VAR(cbind(dinfl.pre1, dunemp.pre1), p = 3, type = "both")
coef(inf_unemp.var)




adf.test(unemp.pre2)
adf.test(infl.pre2)

dunemp.pre2<-diff(unemp.pre2)
dinfl.pre2<-diff(infl.pre2)

adf.test(dunemp.pre2)
adf.test(dinfl.pre2)


po.test(cbind(dinfl.pre2, dunemp.pre2))




inf_unemp.var2 <- VAR(cbind(infl.pre2, unemp.pre2), p = 3, type = "both")
coef(inf_unemp.var2)

plot(dunemp.pre2, dinfl.pre2, pch = 4, main = "Inflation vs unemployment", xlab="Unemployment Change", ylab="Inflation Change")
abline(lm(dinfl.pre2~dunemp.pre2))
cor(dunemp.pre2, dinfl.pre2)


infl_unemp2.pred<-predict(inf_unemp.var2, n.ahead=12)
par(mfrow=c(2,1))
ts.plot(unemp.post2, infl_unemp2.pred$fcst$unemp.pre2[,1], lty=1:2, col=c("red", "blue"), main = "Forecast vs Actual unemployment", xlab="Date Range", ylab="Unemployment")
ts.plot(infl.post2, infl_unemp2.pred$fcst$infl.pre2[,1], lty=1:2, col=c("red", "blue"), main = "Forecast vs Actual inflation", xlab="Date Range", ylab="Inflation")
MAPE(unemp.post2, infl_unemp2.pred$fcst$unemp.pre2[,1])
MAPE(infl.post2, infl_unemp2.pred$fcst$infl.pre2[,1])


irf(inf_unemp.var2, impulse = "unemp.pre2", response = c("infl.pre2"), boot = F)

plot(irf(inf_unemp.var2, impulse = "unemp.pre2", response = "infl.pre2", boot = T, n.ahead = 10, ci=0.95))

