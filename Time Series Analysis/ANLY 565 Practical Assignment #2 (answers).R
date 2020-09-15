# ANLY 580 Time Series and Forecasting 
# Practical Assingment #2
# Value: 150 points
# Deadline: July 1st

#1  Check you working directory
getwd()

#2  Set your working directory to "ANLY 580/RScript". 
setwd("C:/Users/subha/Desktop/ANLY565/RScript")
#   Upload "nlme" library 
install.packages("nlme")
library(nlme)

#3  Download "trade.xls" data file and set the "date" 
#   variable to the date format and the "trade" variable to
#   the numeric format. The "trade" variable represents 
#   the Ratio of Exports to Imports for China expressed in percentages.
library(readxl)
trade<-read_excel("trade.xls", col_types = c("date", "numeric"))

#4  Create two stand alone varibales: "datev" and "tradev". 
#   "datev" variable should represent values of the "date" variable 
#   from the "trade" data set, while, "tradev" variable should represent 
#   values of the "trade" variable from the "trade" data set. 
datev<-trade$date
tradev<-trade$trade

#5  Use the "datev" variable and the range() function to check the time sample
#   covered by the "trade" data set. What time period is covered?
#   What is the frequency of the data?
range(datev)
#The time series range is from Jan 1, 1992 to April 1, 2019. The frequency of the dataset is monthly

#6  Transform "tradev" variable from numeric format to the time series format 
#   by using ts() function. Lable the new varibale as "tradets".  
tradets<-ts(tradev, start = c(1992, 1), freq = 12)

#7  Plot the time series graph of the "tradets"variable.
#   Please lable all axis correcly, and make sure to lable the graph. 
#   Based on this graph does the Ratio of Exports to Imports for China exhibit a trend? 
#   What about a regular seasonal fluctuation? 
plot(tradets, xlab="Date Range", ylab="Ratio of Exports to Imports for China", main="Trade data")
# The chart exhibits a fluctuating curve and there is no apparent trend in the data.
# However, there does seem to be some seasonality in the data.

#8  Use "tradets" variable and window() function to create 2 new variables 
#   called "tradepre", "tradepost". 
#   The "tradepre" should include all observations for the period 
#   up until December 2018.(Last observation should be December 2018)
#   The "tradepost" should include all observations starting from January 2019.
#   and up until the last month in the dataset.
tradepre <- window(tradets, start = c(1992,1), end = c(2018,12), freq = 12)
tradepost <- window(tradets, start = c(2019,1), freq = 12)

#9  Estimate autocorrelation function and partial autocorrelation function for 
#   the "tradepre" variable. Does the trade ratio for China exhibit autocorrelation?  
#   What process can explain this time series (white noise, random walk, AR, etc..)?
acf(tradepre)
acf(tradepre)$acf[2]
pacf(tradepre)


#10 Estimate AR(q) model for the "tradepre" time series. 
#   Use ar() function (set aic=FALSE) and rely on the corellologram 
#   to determine q, the order of the model. Moreover, use maximum liklehood method.
#   After that, set aic=TRUE and estimate ar() again to see if you have identified 
#   the order correctly.
#   Save the estimates as "trade.ar".
ar(tradepre, aic=FALSE, method="mle")
ar(tradepre, aic=TRUE, method="mle")
# Without AIC, the model is estimated to be AR(12). With AIC, the model is estimated to be AR(3)
trade.ar<-ar(tradepre, aic=TRUE, method="mle")

#11 For each of the AR coeficients estimate 95% confidence interval
#   To find 95% confidence intervals you need to add and subtract 2
#   standard deviations of the coefficient estimates. 
#   Hint you can obtain these standard deviations by applying sqrt()
#   function to the diagonal elements of the asymptotic-theory variance 
#   matrix of the coefficient estimates
trade.ar$ar[1] + c(-2, 2)*sqrt(trade.ar$asy.var[1,1])
trade.ar$ar[2] + c(-2, 2)*sqrt(trade.ar$asy.var[2,2])
trade.ar$ar[3] + c(-2, 2)*sqrt(trade.ar$asy.var[3,3])

#12 Extract the residuals from the trade.ar model and estimate 
#   the autocorrelation function. Based on this correlogram would you say 
#   trade.ar model does a good job of explaining the trade ratio in China?
acf(trade.ar$resid, na.action=na.pass)
# Based on the ACF, we can see that lag 1 and beyond are all not statistically significant so this is like white noise.
# Therefore, the AR(3) model does a good job of describing the trade ratio in china


#13 Use trade.ar model and predict() function to creat a 4 period ahead forecast
#   of the trade ratio in China. Save these predicted values as "trade.ar.forc"
trade.ar.forc<- predict(trade.ar, n.ahead=4)


#14 Use ts.plot() function to plot side-by-side actual values of the trade ratio
#   from January 2019-April 2019 period and their forecasted counterparts. 
#   (tradepost and trade.ar.forc)
#   Please designate red color to represent the actual observed values, 
#   and blue doted lines to represent forecasted values. 
#   How does the ability to predict future trade ratio depends on the 
#   time horizon of the forecast?
ts.plot(tradepost, trade.ar.forc$pred, lty = 1:2, col=c("red", "blue"))
# The predictive power of the ratio gets worse, the longer the horizon. 

#15 Please calculate forecast's mean absolute percentage error 
#   for the trade.ar.forc forecasting model. Why is it important to calculate 
#   mean absolute percentage error rather than mean percentage error?
library(MLmetrics)
MAPE(tradepost, trade.ar.forc$pred)
#MAPE is better than MPE because in the case of MPE, positive and negative errors may cancel each other out and
# minimize the error and indicate that the model has a better predictive power than it actually does

#16 Use time() function and tradepre variable to create a variable called "Time".
Time<-time(tradepre)

#17 Estimate linear regression model by regressing "Time" on "tradepre" variable.
#   Save this regression model as "trade.lmt". 
#   By using confint() function calculate 95% confidence intervals for the estimated 
#   model coeficients.
#   What can you conclude based on the estimates of the model coeficients?
#   What is the direction of the time trend?
trade.lmt <- lm(tradepre ~ Time)
confint(trade.lmt)
#We can conclude that the initial value is negative and the direction is positive, i.e. with an increase in time,
# the ratio increases

#18 By visually inspecting a time series plot of the "tradepre" variable, 
#   and given the seasonal nature of the trade relationships it is reasonable to assume 
#   that there are regular seasonal fluctuations in the trade ratio for China. 
#   Use "tradepre" variable and cycle() function to create a factor variable titled "Seas".
Seas <- factor(cycle(tradepre))

#19 Use lm() function to estimate linear regression model by regressing 
#   "Time" and "Seas" on "tradepre". Save this regression model as "trade.lmts".
#   Set the value of the intercept to 0, in order to interpret the 
#   coeficients of the seasonal dummy variables as seasonal intercepts. 
#   (Setting intercept to 0 ensures that for each season there is a unique intercept)
#   What can you conclude based on the estimates of the model coeficients?
#   What is the direction of the time trend? Is there a seasonal component?
#   During which month should you expect the trade ratio to be the largest?
trade.lmts <- lm(tradepre ~ 0 + Time + Seas)
coef(trade.lmts)
# We can conclude that the direction of trend is positive, i.e. increasing time leads to higher ratios
# The month of November should see the highest ratio

#20 Extract the residual series from the "trade.lmts" model and save them as 
#   "trade.lmts.resid". Then, estimate autocorrelation function to check the 
#   goodness of the fit. What is the value of autocorrelation at lag 1?
#   What can you conclude based on the correlogram of the residual series?
trade.lmts.resid<-trade.lmts$residuals
acf(trade.lmts.resid)
acf(trade.lmts.resid)$acf[2]
# we can conclude that the residual still has autocorrelation and the seasonality and time trend does not capture
# all of the variability in the data

#21 Fit linear model by regressing "Time" and "Seas" on "tradepre"
#   by uzing generalized least squares (gls() fucntion).
#   Set the value of the intercept to 0, in order to interpret the 
#   coeficients of the seasonal dummy variables as seasonal intercepts.
#   Save this model's estimates as "trade.gls".
trade.gls <- gls(tradepre ~ 0 + Time + Seas, corr=corARMA(p=3, q=0))

#22 Compute Akaike's An Information Criterion for "trade.lmts" and "trade.gls".
#   Which model performs better?
AIC(trade.lmts)
AIC(trade.gls)
#AIC value of GLS model is lower, therefore it performs better. 

#23 Create the following new variables: 
#   "new.Time"- sequence of 4 values starting from 2019 and each number going up by 1/12
#   "alpha" - assumes value of the Time coeficient from the trade.gls model
#   "beta" - takes on values of the first, second, third, and fourth seasonal coeficients 
#            from the trade.gls model.
new.Time <- seq(2019, len = 4, by = 1/12)
alpha <- coef(trade.gls)[1]
beta <- coef(trade.gls)[2:5]


#24 By using the forecasting equation of x_(t+1)<-0+alpha*Time_(t+1)+beta
#   create a 4 period ahead forecast of the trade ratio for China. 
#   Lable this forecast as "trade.gls.forc"
trade.gls.forc<-(alpha * new.Time + beta)[1:4]

#25 Use ts.plot() function to plot side-by-side actual values of the trade ratio
#   from January 2019-April 2019 period and their forecasted counterparts. 
#   (tradepost and trade.gls.forecast)
#   Please designate red color to represent the actual observed values, 
#   and blue doted lines to represent forecasted values.
ts.plot(tradepost, trade.gls.forc, lty = 1:2, col=c("red", "blue"))


#26 Please calculate forecast mean absolute percentage error 
#   for the "trade.gls.forc" forecasting model. Based on the 
#   forecast's mean absolute percentage error, which of the two models, 
#   "trade.ar.forc" and trade.gls.forc" performs better?
MAPE(tradepost, trade.gls.forc)
MAPE(tradepost, trade.ar.forc$pred)
# trade.ar.force is performing better than the GLS model

#27 Create a variable called tradepreL, that represents the first lagged value
#   of the "tradepre" variable. For example tradepreL_t=tradepre_(t-1).
#   Moreover, transform "tradepreL" variable into a time series object by using ts().
#   It should cover the same time period as "tradepre".
tradepreL<-stats::lag(tradepre,1)

#28 Use lm() function to estimate linear regression model by regressing 
#   "tradepreL", "Time" and "Seas" on "tradepre". 
#   Set the value of the intercept to 0, in order to interpret the 
#   coeficients of the seasonal dummy variables as seasonal intercepts.
#   Save this regression model as "trade.ar.lmts".
trade.ar.lmts<-lm(tradepre ~ 0 + Time + Seas + tradepreL)
coef(trade.ar.lmts)
# Coefficient of tradepreL is 1, so it is a seasonal random walk with trend. Coefficient of time is negative,
# so trend is decreasing with time

#29  By using new.Time variable, and the following forecasting equation 
#    x_(t+1)<-0+alpha1*x_t+alpha2*Time_(t+1)+beta 
#    create the following new variables:
#   "alpha1" - assumes value of the tradepreL coefiencient from the trade.ar.lmts model 
#   "alpha2" - assumes value of the Time coeficient from the trade.ar.lmts model
#   "beta1" - takes on values of the first seasonal coeficient from the trade.ar.lmts.
#   "beta2" - takes on values of the second seasonal coeficient from the trade.ar.lmts.
#   "beta3" - takes on values of the third seasonal coeficient from the trade.ar.lmts.
#   "beta4" - takes on values of the fourth seasonal coeficient from the trade.ar.lmts.
#   "forc20191" - takes on the forecasted value of the trade ratio for January 2019
#   "forc20192" - takes on the forecasted value of the trade ratio for February 2019
#   "forc20193" - takes on the forecasted value of the trade ratio for March 2019
#   "forc20194" - takes on the forecasted value of the trade ratio for April 2019
#   "trade.ar.lmts.forc" a vector of four predicted trade ratios.
coef(trade.ar.lmts)[2]
alpha1 <- coef(trade.ar.lmts)[14]
alpha2 <- coef(trade.ar.lmts)[1]
beta1  <- coef(trade.ar.lmts)[2]
beta2  <- coef(trade.ar.lmts)[3]
beta3  <- coef(trade.ar.lmts)[4]
beta4  <- coef(trade.ar.lmts)[5]

forc20191<-(alpha1 * (new.Time-1) + alpha2 * new.Time + beta1)[1]
forc20192<-(alpha1 * (new.Time-1) + alpha2 * new.Time + beta2)[2]
forc20193<-(alpha1 * (new.Time-1) + alpha2 * new.Time + beta3)[3]
forc20194<-(alpha1 * (new.Time-1) + alpha2 * new.Time + beta4)[4]
trade.ar.lmts.forc<- c(forc20191, forc20192, forc20193, forc20194)

#30 Please calculate forecast mean absolute percentage error 
#   for the trade.ar.lmts.forc forecasting model.
#   Which of the following models would you chose to based on this criteria?
#   Models: trade.ar.forc, trade.gls.forc, and trade.ar.lmts.forc)
MAPE(tradepost, trade.ar.lmts.forc)
MAPE(tradepost, trade.gls.forc)
MAPE(tradepost, trade.ar.forc$pred)
#The AR model is the best predictive model of the three. 