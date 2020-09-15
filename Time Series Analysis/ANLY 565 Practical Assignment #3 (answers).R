# ANLY 580 Time Series and Forecasting 
# Practical Assingment #3
# Value: 150 points
# Deadline: July 25th

#1  Check your working directory
getwd()


#2  Set your working directory to "ANLY 580/RScript". 
setwd("C:/Users/subha/Desktop/ANLY565/RScript")

#3  Download "Inflation.xls" data file and set the "observation_date" 
#   variable to the date format and the "CPI" variable to the numeric format.
#   The "CPI" variable represents the consumer price index,
#   which idicates the relative prices of a consumer basket. 
library(readxl)
inflation<-read_excel("inflation.xls", col_types = c("date", "numeric"))


#4  Create two stand alone varibales: "date" and "cpi". 
#   "date" variable should represent the values of the "observation_date" 
#   variable from the "Inflation" data set, while, "cpi" variable should represent 
#   values of the "cpi" variable from the "Inflation" data set.
date<-inflation$observation_date
cpi<-inflation$CPI


#5  Transform "cpi" variable from numeric format to the time series format 
#   by using ts() function. Lable the new varibale as "cpits".  
cpits<-ts(cpi, start=c(1913,1), freq=12)


#6  Please construct the following three graphs:
#   1)time series plot, 2) autocorrelation
#   and 3) partial autocorrelation functions for the "cpits" variable. 
#   Based on the signature of these graphs, does the variable appear
#   stationary? Explain
plot(cpits, xlab="Date Range", ylab="CPI", main="Inflation data")
acf(cpits)
pacf(cpits)
# No, the variable does not appear stationary, because the ACF is decaying very slowly implying that
# the autocorrelation is a function of time


#7  Use "cpits" variable and window() function to create 2 new variables 
#   called "cpi.pre", "cpi.post". 
#   The "cpi.pre" should include all observations for the period starting from 
#   January of 1990 and up until October 2018.
#   The "cpi.post" should include all observations starting from November 2018.
#   and up until the last month in the dataset.
cpi.pre <- window(cpits, start = c(1990,1), end = c(2018,10), freq = 12)
cpi.post <- window(cpits, start = c(2018,11), freq = 12)


#8  Use time() function and "cpi.pre" variable to create a variable called "Time".
#   Moreover, use "cpi.pre" variable and cycle() function to create 
#   a factor variable titled "Seas".
Time<-time(cpi.pre)
Seas <- factor(cycle(cpi.pre))



#9  Use lm() function to estimate parameter values of a linear regression model 
#   by regressing "Time", and "Seas" on "cpi.pre". 
#   Save these estimates as "cpi.lm".
#   Set the value of the intercept to 0, in order to interpret the 
#   coeficients of the seasonal dummy variables as seasonal intercepts. 
#   (Setting intercept to 0 ensures that for each season there is a unique intercept) 
#   Save these estimates as cpi.lm
cpi.lm <- lm(cpi.pre ~ 0 + Time + Seas)
summary(cpi.lm)

#10 Create the following new items: 
#   "new.Time"- sequence of 12 values starting from 2018.75+1/12
#    and each number going up by 1/12
#   "new.Seas"- a vector with the following values c(11,12,1,2,3,4,5,6,7,8,9,10)
#   "new.data"- a data frame that combines the "new.Time" and "new.Seas" variables.
new.Time <- seq(2018.75+1/12, len = 12, by = 1/12)
new.Seas <- c(11,12,1,2,3,4,5,6,7,8,9,10)
new.data <- data.frame(Time=new.Time, Seas=as.factor(new.Seas))

#11 Use predict() function and cpi.lm model to create a 12 month ahead forecast 
#   of the consumer price index. Save this forecast as "predict.lm"
predict.lm <- predict(cpi.lm, new.data)


#12 Collect residuals from the "cpi.lm" model and save them as "cpi.lm.resid".
#   Moreover, constuct acf and pacf for the "cpi.lm.resid" series. 
#   Is the series stationary?
#   Is there autocorrelation in the residual series?
cpi.lm.resid <- cpi.lm$residuals
acf(cpi.lm.resid)
pacf(cpi.lm.resid)
# The slow decay in ACF indicates non-stationary series
# There is autocorrelation in the residual series


#13 Based on the AIC, identify the best order of ARMA model 
#   (without the seasonal component) for the cpi.lm.resid time series 
#   and estimate the value of the parameter coefficients. 
#   Please, consider any ARMA model with up to 3 AR and/or MA terms.
#   Save these estimates as resid.best.arma.
#   What is the order of resid.best.arma?
best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:3) for (j in 0:3) {
  fit.aic <- AIC(arima(cpi.lm.resid, order = c(i, 0, j)))
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    resid.best.arma <- arima(cpi.lm.resid, order = best.order)
    best.aic <- fit.aic
  }}
resid.best.arma
#Order of resid.best.arma is ARMA(1,2) --> AR(1) and MA(2)


#14 Use predict() function and resid.best.arma to 
#   create a 12 period ahead forecast of cpi.lm.resid series.
#   Save the forecasted values as resid.best.arma.pred
resid.best.arma.pred <- predict(resid.best.arma, n.ahead=12)


#15 Use ts() function to combine the cpi values forecaseted by cpi.lm model
#   and the residual values forecasted by resid.best.arma.
#   Lable this time series as cpi.pred
cpi.pred <- ts(predict.lm + resid.best.arma.pred$pred, start=c(2018,11), freq=12)

#16 Use ts.plot() function to plot cpi.pre and cpi.pred together on one graph.
#   What do you expect will happen to the CPI during the next 12 month?
ts.plot(cpi.pre, cpi.pred, lty = 1:2, col=c("red", "blue"))
#We expect CPI to go up in the next 12 months

#17 Please calculate mean absolute percentage error for the cpi.pred
#   forecast for the first three month (Novermber 2018, December 2018, January 2019)
#   How accurate is the model? 
library(MLmetrics)
MAPE(cpi.post[1:3], cpi.pred[1:3])
#Given the very low MAPE of 0.0053 the model is fairly accurate

#18 What is the forecasted rate of inflation between December 2018 and January 2019?
#   Hint: Inflation = % change in CPI
forecast_infl <- (cpi.pred[3]/cpi.pred[2])-1
forecast_infl
# Forecasted inflation between Dec 18 and Jan 19 is 0.284%

#19 Policy makers often care more about inflation rather than cpi.
#   Create a new stand alone varible that would represent 
#   the first log difference of the the cpits variable. 
#   Lable this variable  "pi", which represents monthly inflation rate in the US.
#   If percentage change is positive there is inflation (prices go up), 
#   and if the percentage change is negative there is deflation (prices fall). 
#   What was the lowest monthly rate of inflation(deflation) recorded in US
#   during the time sample? What about was the highest?
pi<-diff(log(cpits))
min(pi)
# Lowest monthly inflation was -3.2%
max(pi)
# Higest monthly inflation was 5.7%

#20 Please construct the time series plot, the autocorrelation
#   and partial autocorrelation functions for the "pi" variable. 
#   Based on the signature of these graphs, does the variable appear
#   stationary? Explain
plot(pi)
acf(pi)
pacf(pi)
# Yes ACF decays to 0 at higher lags indicating stationarity

#21 Use "pi" variable and window() function to create 2 new variables 
#   called "pi.pre", "pi.post". 
#   The "pi.pre" should include all observations for the period starting from 
#   January of 1990 and up until October 2018.
#   The "pi.post" should include all observations starting from November 2018.
#   and up until the last month in the dataset.
pi.pre <- window(pi, start = c(1990,1), end = c(2018,10), freq = 12)
pi.post <- window(pi, start = c(2018,11), freq = 12)


#22 Please create a function that takes a time series as input, 
#   and then uses AIC to identify the best SARIMA model. 
#   The function should return the following:
#   - the order of the best SARIMA, 
#   - its AIC
#   - and the estimates of its coefficient values
#   Lable this formula get.best.sarima
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

#23  By using get.best.sarima() function please identify the best SARIMA model
#    for pi.pre time series. 
#    Please cosider SARIMA(2,2,2,2,2,2) as the maximum order of the model. 
#    Save the results of the get.best.sarima() function as "pi.best.sarima"
#    What is the order of the best SARIMA model?
pi.best.sarima<-get.best.sarima(pi.pre, maxord = c(2,2,2,2,2,2))
pi.best.sarima[[3]]
# It is SARIMA (2,0,0,2,0,1) 

#24  Please use predict() function and the best.sarima.pi model to forecast
#    monthly rate of inflation in the US during November 2018, December 2018
#    and January 2019.
#    Save these predictions as pi.sarima.pred
pi.sarima.pred<-predict(pi.best.sarima[[2]], n.ahead=3)

#25 Please calculate mean absolute percentage error of the best.sarima model.
#   How accurate is the model? 
MAPE(pi.post[1:3], pi.sarima.pred$pred[1:3])
#MAPE is 0.85 which is greater than that for CPI forecast, the model is less accurate

#26 Extract the residual series from the pi.best.sarima model,
#   and save them as sarima.resid.
sarima.resid <- pi.best.sarima[[2]]$residuals


#27 Plot the acf of the sarima.resid series and acf of the residsarima.resid^2 series 
#   What can you conclude based on these graphs?
acf(sarima.resid)
acf(sarima.resid^2)
#The ACF of the residual exhibits no autocorrelation but the square of the residual is exhibiting autocorrelation.
#This implies that there is heteroskedasticity in the data

#28 Download fGarch package and upload it to the library
install.packages("fGarch")
library(fGarch)

#29 Use garchFit() function from the fGarch package 
#   to estimate garch(1,1) model of the resid time series. 
#   By doing so you will be able to analyze the volatility of the 
#   inflation, or, in other words,  how stable it is.
#   Make sure to set "include.mean=F" by doing so you suppress mean parameter 
#   from the default ARMA(0,0) model.
#   Save the estimated coefficients as resid.garch
resid.garch<-garchFit(~garch(1,1), data=sarima.resid,include.mean = F, trace = F)
resid.garch

acf(resid.garch@residuals)
acf(resid.garch@residuals^2)

#30 The main priority of the monetary authority (Federal Reserve)
#   in the United States is to ensure stable value of currency. 
#   Simply put, Fed wants to keep inflation stable (no volatility). 
#   To maintain stability the Fed depends on a number of tools, 
#   and its effectiveness is judged based on the forecasting model of volatility.
#   Please use pi variable and predict function 
#   to forecast two period ahead inflation volatily, which is measured by 
#   a square of the forecasted standard deviation.
#   How stable will be the currency in February and March of 2019?

# Predict inflation for Nov 2018 to Jan 2019
pi.best.sarima.pred <- predict(pi.best.sarima[[2]], n.ahead=3)
pi.best.sarima.pred
# Take difference from pi.post to get the forecast error
pi.forecast.error <- pi.post - pi.best.sarima.pred$pred
pi.forecast.error [1]
pi.forecast.error [2]
pi.forecast.error [3]

resid.garch

#Forecast equation:
#con.var_t = 0.364*w_(t-1)^2 + 0.72*con.var_(t-1)

#con.var_Feb
con.var_feb = 0.364*pi.forecast.error[2]^2
con.var_mar = 0.364*pi.forecast.error[3]^2 + con.var_feb

con.var_feb
con.var_mar
#The volatility is going up in February and March, so the currency is less stable