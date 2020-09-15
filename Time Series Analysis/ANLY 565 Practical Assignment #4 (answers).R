# ANLY 580 Time Series and Forecasting 
# Practical Assingment #4
# Value: 150 points
# Deadline: August 8th

#1  Check your working directory
getwd()

#2  Set your working directory to "ANLY 580/RScript". 
setwd("C:/Users/subha/Desktop/ANLY565/RScript")

#3  Download "ffrategdp.xls" data file and set the "observation_date" 
#   variable to the date format and the "FEDFUNDS" and "GDPC1" variables to the numeric format.
#   The "FEDFUNDS" variable represents the effective federal funds rate,
#   which idicates the interest rate at which depository institutions trade federal funds 
#   (balances held at Federal Reserve Banks) with each other overnight. 
#   The "GDPC1" variable represents real gross domestic product.
library(readxl)
gdp_rate<-read_excel("ffrategdp.xls", col_types = c("date", "numeric", "numeric"))

#4  By using ts() function create a time series object that contains two variables: "FEDFUNDS" and "GDPC1".
#   Label it as "ffrategdpts".
ffrategdpts<-ts(gdp_rate[,2:3], start=c(1954,3), freq=4)

#5  Create two stand alone variables "fedrate" and "gdp" that take on values of the "FEDFUNDS" and "GDPC1"
#   variables from the "ffrategdpts" data set.
fedrate<-ffrategdpts[,1]
gdp<-ffrategdpts[,2]

#6  When the federal funds rate goes down, the comercial loan interest rates go down too.
#   This means that people can borrow cheaply and invest in their businesses, 
#   which will result in higher gross domestic product. 
#   Therefore, you suspect that the federal funds rate has a negative correlation with GDP.
#   To test this hypothesis you decide to use lm() function to estimate the coeficients of 
#   a linear regression model in which "gdp" is a dependent variable and "fedrate" 
#   is an idependent variable.  
#   Save the estimated model as gdpfr.lm
#   Based on the results of this model can you make any conclusions about the nature of the 
#   relationship between the gdp and the federal funds rate?
gdpfr.lm<-lm(gdp~fedrate)
summary(gdpfr.lm)
#The negative coefficient of fedrate suggests that there is a negative correlation between GDP and Fed funds rate

#7  You have suspected that the "gdp" variable may contain a unit root.
#   By using Augmented Dickey Fuller method test "gdp" variable for the 
#   the presence of unit root. 
#   Does "gdp" variable contain a unit root?
#   Is "gdp" variable stationary?
adf.test(gdp)
# p-value indicates that we cannot reject null hypothesis of a unit root, Therefore it is not stationary.

#8  By using Augmented Dickey Fuller method test "fedrate" variable for the 
#   the presence of unit root. 
#   Does "fedrate" variable contain a unit root?
#   Is "fedrate" variable stationary?
adf.test(fedrate)
# p-value indicates that we cannot reject null hypothesis of a unit root, Therefore it is not stationary.

#9  The Phillips-Ouliaris test shows whether there is evidence that the series are
#   cointegrated, which justifies the use of a regression model. 
#   Are "gdp" and "fedrate" variables cointegrated?
#   Is "gdpfr.lm" a suitable model to explore the relationship between "gdp" and "fedrate"?
po.test(cbind(gdp, fedrate))
# p-value indicates that the two variables are not cointegrated. Therefore, building a standard linear regression model
# like in gdpfr.lm s not suitable to explore the relationship between these variables


#10 Create the following 2 new variables:
#   "gdpgrowth" - that represents quarterly percentage chagne in GDP
#   "fedratediff" - that represents quarterly difference in the federal funds rate (simple difference)
#   To each of the varibles add "NA" as the first observation .
#   This will ensure that the new variables are of the same length as the existing variables.
fedratediff<-diff(fedrate)
gdpgrowth<-diff(gdp)/lag(gdp,-1)

#11  By using ts() and cbind() functions add "gdpgrowth" and "fedratediff" variables 
#    to the "ffrategdpts" data set. 
ffrategdpts<-cbind(ffrategdpts, gdpgrowth, fedratediff)

#12 Use na.omit() function to get rid of the missing values in the "ffrategdpts" data set. 
#   Save the new data set as "ffrategdptscc". 
ffrategdptscc<-na.omit(ffrategdpts)

#13  Create 2 new variables: 
#    "ggdp" - takes on values of the "gdpgrowth" from the "ffrategdptscc"
#    "dfrate" - takes on values of the "fedratediff" from the "ffrategdptscc"
ggdp<-ffrategdptscc[,3]
dfrate<-ffrategdptscc[,4]


#14 Use to Augmented Dickey-Fuller test to determine whether "ggdp" and "dfrate"
#   are statiomary or not. 
#   Does "ggdp" contain a unit root? Is "ggdp" stationary? 
#   Does "dfrate" contain a unit root? Is "dfrate" stationary?
adf.test(ggdp)
# p-value is < 0.01, therefore, ggdp does not contain a unit root and is stationary
adf.test(dfrate)
# p-value is < 0.01, therefore, dfrate does not contain a unit root and is stationary


#15 Use lm() function to estimate the coeficients of a linear regression model 
#   in which "ggdp" is a dependent variable and "dfrate" is as an idependent variable.
#   Lable these estimates as "ggdp.dfrate.lm".
#   Based on the findings of the linear regression model what is the nature of the relationship 
#   between the growth rate of real gdp and difference in federal funds rate?
ggdp.dfrate.lm<-lm(ggdp~dfrate)
summary(ggdp.dfrate.lm)
#Based on the coefficient of the dfrate variable in the regression, there exists a positively correlated
# relationship between ggdp and dfrate


#16 Create a variable called "ggdp.dfrate.lm.resid" that represents the residual series obtained 
#   from the "ggdp.dfrate.lm" regression
ggdp.dfrate.lm.resid<-ggdp.dfrate.lm$residuals


#17 Constract acf and pacf functions for "ggdp.dfrate.lm.resid".
#   What can you say about the goodness of the fit of the model?
acf(ggdp.dfrate.lm.resid)
pacf(ggdp.dfrate.lm.resid)
# The ACF and PACF shows evidence of autocorrelation of AR1 type. Therefore, the regression model does not fully
# capture the relationship

#18 Maybe vector autoregression model would prove a better fit. 
#   Upload "vars" library that contains VAR() function
library(vars)

#19 Estimate a VAR model for the "ggdp" and "dfrate" variables.
#   In this modelincludes 3 lags of each variable. 
#   Save the estimates of the var model as "ggdp.dfrate.var"
ggdp.dfrate.var <- VAR(cbind(ggdp, dfrate), p = 3)


#20 Use plot() and irf() functions to obtain and plot impulse response functions for each variable.
#   IRF illustrates the behavior of a variable in response to one standard deviation shock 
#   in its own value and in the value of the other variable.
#   Based on the these graph what conclusions can you draw about the nature of the relationship between 
#   the growth rate of gdp and the differene in federal funds rate? 
#   Any potential explanations?
plot(irf(ggdp.dfrate.var, impulse = "dfrate", response = "ggdp", boot = T, n.ahead = 10, ci=0.95))
plot(irf(ggdp.dfrate.var, impulse = "ggdp", response = "dfrate", boot = T, n.ahead = 10, ci=0.95))
#Based on the plot for response in gGDP to impulse in dfrate, we can say that a 1% increase in difference of federal
# funds rate leads to a drop in GDP growth with the max drop occuring by lag 3, roughly by 0.2% - 0.25%

#Based on the plot for response in dfrate to impulse in ggdp, we can say that a 1% increase in GDP leads to an
# increase in the difference in fed funds rate by roughly 20-25% by lag 2

#21 Use resid() function to obtain the residuals from the ggdp equation of the "ggdp.dfrate.var" model.
#   Save this residual series as "var.ggdp.resid".
var.ggdp.resid<-resid(ggdp.dfrate.var$varresult$ggdp)


#22 #17 Use resid() function to obtain the residuals from the dfrate equation of the "ggdp.dfrate.var" model.
#   Save this residual series as "var.dfrate.resid".
var.dfrate.resid<-resid(ggdp.dfrate.var$varresult$dfrate)


#24 Plot acf and pacf functions for the 'var.ggdp.resid". 
#   Does "ggdp.dfrate.var" model provide a good fit to explain growth rate of gdp?
acf(var.ggdp.resid)
pacf(var.ggdp.resid)

# The ggdp VAR residual plot shows no autocorrelation in the ACF plot and all the spikes in PACF are statistically
# insignificant. Therefore, the VAR model does a good job of explaining growth rate of GDP


#25 Plot acf and pacf functions for the 'var.dfrate.resid". 
#   Does "ggdp.dfrate.var" model provide a good fit to explain the difference in federal funds rate?
acf(var.dfrate.resid)
pacf(var.dfrate.resid)

# The dfrate VAR residual plot shows a slight evidence of autocorrelation at lag 5. But it is barely significant
# We can conclude that the VAR model does a good job of explaining the difference in federal funds rate


#26 Use "ggdp.dfrate.var" model and predict() function to forecast growth rate of gdp and 
#   change in federal funds rate over the upcoming year. 
#   Save the predicted values as "VAR.pred"
VAR.pred <- predict(ggdp.dfrate.var, n.ahead = 4)


#27 Use ts() function and VAR.pred forecast to create a new variable "ggdp.pred".
#   It should contain the forcasted values of the growth rate of gdp over the next 4 quarters.
ggdp.pred <- ts(VAR.pred$fcst$ggdp[,1], start=c(2019,3), freq=4)
ggdp.pred


#28 Use ts() function and VAR.pred forecast to create a new variable "dfrate.pred".
#   It should contain the prediction of the change in the federal funds rate over the next 4 quarters.
dfrate.pred <- ts(VAR.pred$fcst$dfrate[,1], start=c(2019,3), freq=4)
dfrate.pred


#29 Plot the times series graph of the past growth rates of gdp alongside 
#   its future forecasted values. Do you expect the gdp to grow over the next 4 quarters?
#   Is a new recession likely to happen in the coming year?
ts.plot(ggdp, ggdp.pred, lty=1:2, col=c("red", "blue"), main = "Forecast vs Actual GDP Growth Rate", 
        xlab="Date Range", ylab="GDP growth rate")
#No, GDP growth rate is positive for next 4 quarters, so there will be no recession in next 4 quarters based on forecast


#30 Plot the times series graph of the past changes of the federal funds rate alongside 
#   its future forecasted values. 
#   Do you expect the federal funds rate to increase over the next 4 quarters?
#   Should one take out a loan now?
ts.plot(dfrate, dfrate.pred, lty=1:2, col=c("red", "blue"), main = "Forecast vs Actual Changes in Fed Funds Rate", 
        xlab="Date Range", ylab="Changes in Fed Funds Rate")
#The federal funds rate is expected to go down in the next 4 quarters. Therefore, it would be advisable to wait
# and not take out a loan right now but 4 quarters down the line