# ANLY 565 Time Series and Forecasting 
# Practical Assingment #1 
# Value: 150 points
# Deadline: June 6th

#1  Check you working directory
getwd()

#2  Set your working directory to "ANLY 565/RScript"
setwd("C:/Users/subha/Desktop/ANLY565/RScript")

#3  Download goy data set posted on Moodle  and lable it 
#   goy. This data set reperesnets daily prices of gold,
#   oil, and the price of 1 US dollar in terms of Japanese yen.
#   Set the first column in each data set to the date format 
#   and the remaining columns in numerical format.
install.packages("readxl")
library(readxl)
goy<-read_excel("goy.xls", col_types = "date", "numeric", "numeric", "numeric")

#4  Create a new data set called "goycc" that contains all complete caises of goy data.
#   Utilize complete.cases function.
goycc<-goy[complete.cases(goy),]

#5  Create a stand alone variable "date" that takes on values of "observation_date"
#   variable from the goycc data set. Set the mode of the varible to character
date<-goycc$observation_date
date<-as.character(date)

#6  Find the range of dates covered in goycc data set by applying range() function
#   to "date" variable. 
range(date)

#7  Create a time series objected called "goyccts" by utilizing goycc dataset and 
#   ts() function. In this dataset please exclude the first column 
#   of the goycc dataset. 
goyccts<-ts(goycc[,2:4], start = c(1971, 1), freq = 12)

#8  Reasign the value of the yen varible from the goyccts data set
#   by conventing the exchange rate of yen that represents 
#   the price of 1 US Dollar in terms of Japanese yen to represent 
#   the price of 1 Yen in terms of US Dollar. 
#   This way if the number increases it represent appriciation of Yen. 
#   Hint: Reasign the value of yen variable by taking a reciprocal. 
goyccts[,3]<- (1/goyccts[,3])

#9  Plot the time series plot of the three assets. Do you see any trend?
#   Do you see any seasonal component?
plot(goyccts)
# There seems to be an increasing trend in all the three charts. Seasonal components are not immediately apparent

#10 Utilize the aggregate function to plot annual prices of the three assets.
#   How does this graph differ from the monthly time series plot?
plot(aggregate(goyccts))
#The graph is a lot smoother. The data now shows an increasing trend without the within year fluctuations in monthly plot

#11 Find the average summer price of oil for the entire sample.
goyccts_sum<-goyccts[month.abb[cycle(goyccts[,2])] %in% c("Jun", "Jul", "Aug")]
a<-mean(goyccts_sum)

#12 Find the average winter price of oil for the entire sample.
goyccts_win<-goyccts[month.abb[cycle(goyccts[,2])] %in% c("Oct", "Nov", "Dec")]
b<-mean(goyccts_win)

#13 How does the summer price of oil compare to the winter price of oil.
#   Please provide your answer in percentages. 
(a/b-1)*100

#14 Use window() function to create three stand alone variables 
#   "gold", "oil", and "yen" that take on values of the "gold", "oil", and "yen" 
#   variables from the goyccts dataset starting from January of 2005
gold <- window(goyccts[,1], start = c(2005,1), freq = 12)
oil  <- window(goyccts[,2], start = c(2005,1), freq = 12)
yen  <- window(goyccts[,3], start = c(2005,1), freq = 12)

#15 Use plot() and decompose() functions to generate three graphs that would depict
#   the observed values, trends, seasonal, and random components for "gold"
#   "oil" and "yen" variables. Would you choose multiplicative or 
#   additive decomposition model for each of the variables?
plot(decompose(gold))
plot(decompose(oil))
plot(decompose(yen))
#we would choose additive decomposition for these variables because there is no indication from the plots 
#that seasonal effect increases/decreases with time

#16 For each of the variables extract the random component and save 
#   them as "goldrand", "oilrand", and "yenrand". Moreover, use na.omit()
#   function to deal with the missing values.
goldrand<-na.omit(decompose(gold)$random)
oilrand<-na.omit(decompose(oil)$random)
yenrand<-na.omit(decompose(yen)$random)

#17 For the random component of each of the assets, please estimate 
#   autocorrelation function.Does any of the assets exhibit autocorrelation?
#   If yes, to what degree?
#   Keep in mind there are missing values. 
acf(goldrand)
acf(goldrand)$acf[2]
pacf(goldrand)
#Gold exhibits autocorrelation. Upto 1 lag with a correlation of 0.619
acf(oilrand)
acf(oilrand)$acf[2]
pacf(oilrand)
#Oil exhibits autocorrelation. There is evidence for autocorrelation with 2 lags with 0.8 at lag1 and -0.4 at lag2
acf(yenrand)
pacf(yenrand)
#Yen exhibits autocorrelation. there is evidence of autocorrelation upto 3 lags

#18 For all possible pairs of assets please estimate cross-correlation function 
#   Do any of the variable lead or precede each other?
#   Could you use any of the varibales to predict values of other variables?
#   Make sure to use detranded and seasonally adjusted variables. 
#   ("goldrand", "oilrand", and "yenrand")
ccf(ts(goldrand), ts(oilrand))
print(ccf(ts(goldrand), ts(oilrand)))
#gold leads oil by 4 periods (months) as can be seen from the peak of 0.321 at a lag of -4
ccf(ts(goldrand), ts(yenrand))
print(ccf(ts(goldrand), ts(yenrand)))
#Gold lags Yen by 1 period as can be seen from the peak 0.492 at lag 1
ccf(ts(oilrand), ts(yenrand))
print(ccf(ts(oilrand), ts(yenrand)))
#Oil lags Yen by 4 periods as can be seen from the peak of 0.377 at lag 4
#We can use Gold to predict oil, Yen to predict Gold and Oil

#19 Based on the time series plot of gold, oil, and yen prices, 
#   there appears to be no systematic trends or seasonal effects. 
#   Therefore, it is reasonable to use exponential smoothing for these time series.
#   Estimate alpha, the smoothing parameter for gold, oil and yen. 
#   What does the value of alpha tell you tell you about the behavior of the mean? 
#   What is the estimated value of the mean for each asset?
gold.hw1 <- HoltWinters(gold, beta =F, gamma = F) 
gold.hw1
# The high value of alpha of 0.999 suggests there is almost no smoothing for Gold and mean at time is roughly
# the same as mean at time t-1

oil.hw1 <- HoltWinters(oil, beta =F, gamma = F) 
oil.hw1
# The high value of alpha of 0.999 suggests there is almost no smoothing for Oil and mean at time is roughly
# the same as mean at time t-1

yen.hw1 <- HoltWinters(yen, beta =F, gamma = F) 
yen.hw1
# The high value of alpha of 0.999 suggests there is almost no smoothing for yen and mean at time is roughly
# the same as mean at time t-1

#20 Use plot() function to generate three graphs that depict observed 
#   and exponentially smoothed values for each asset.
plot(gold.hw1)
plot(oil.hw1)
plot(yen.hw1)

#21 Use window() function to create 3 new variables called 
#   "goldpre", "oilpre", and "yenpre" that covers the period from January 2005, 
#   until August 2018. 
goldpre <- window(goyccts[,1], start = c(2005,1), end = c(2018,8), freq = 12)
oilpre  <- window(goyccts[,2], start = c(2005,1), end = c(2018,8), freq = 12)
yenpre  <- window(goyccts[,3], start = c(2005,1), end = c(2018,8), freq = 12)

#22 Use window() function to create 3 new variables called 
#   goldpost, oilpost, and yenpost that covers the period from September 2018, 
#   until February 2019.
goldpost <- window(goyccts[,1], start = c(2018,9), freq = 12)
oilpost  <- window(goyccts[,2], start = c(2018,9), freq = 12)
yenpost  <- window(goyccts[,3], start = c(2018,9), freq = 12)

#23 Estimate HoltWinters filter model for each asset, while using only only pre data.
#   Save each of these estimates as "gold.hw", "oil.hw", and "yen.hw".
gold.hw <- HoltWinters(goldpre)
oil.hw  <- HoltWinters(oilpre)
yen.hw  <- HoltWinters(yenpre)

#24 Use HoltWinters filter estimates generated in#23 and predict() function 
#   to create a 6 month ahead forecast of the gold, oil, and yen prices. 
#   Save these forcasted values as "goldforc", "oilforc", and "yenforc".

goldforc<- predict(gold.hw, n.ahead=6)
oilforc<-  predict(oil.hw, n.ahead=6)
yenforc<-  predict(yen.hw, n.ahead=6)

#25 Use ts.plot() function to plot side-by-side post sample prices 
#   ("goldpost", "oilpost","yenpost") and their forecasted counterparts.
#   Please designate red color to represent the actual prices, 
#   and blue doted lines to represent forecasted values. 
ts.plot(goldpost, goldforc, lty = 1:2, col=c("red", "blue"))
ts.plot(oilpost, oilforc, lty = 1:2, col=c("red", "blue"))
ts.plot(yenpost, yenforc, lty = 1:2, col=c("red", "blue"))

#26 Please calculate forecast mean percentage error for each assets forecasting model. 
#   Which asset's forecasting model has the lowest mean percentage error?
install.packages("MLmetrics")
library(MLmetrics)
MAPE(goldpost, goldforc)#0.065
MAPE(oilpost, oilforc)#0.099
MAPE(yenpost, yenforc)#0.039
#Yen's forecasting model has the lowest mean percentage error

#27 Use gold, oil, and yen variables to estimate HoltWinters model
#   for each asset. Save these estimates as "goldc.hw", "oilc.hw", and "yenc.hw".
goldc.hw <- HoltWinters(gold)
oilc.hw  <- HoltWinters(oil)
yenc.hw  <- HoltWinters(yen)

#28 Use "goldc.hw", "oilc.hw", and "yenc.hw" models to create an out-of-sample
#   forecasts to predict the prices of each of the assets for the rest of the 2019.
#   Save these forecasts as "goldforcos", "oilforcos", "yenforcos".
#   What is the forecasted price of Gold for November 2019? 
goldforcos<- predict(goldc.hw, n.ahead=10)
oilforcos<-  predict(oilc.hw, n.ahead=10)
yenforcos<-  predict(yenc.hw, n.ahead=10)

goldforcos[9]
# Price of Gold forecasted for Nov 19 is 1276.936

#29 Create time series plots for each asset, that combines the actual price data
#   of each asset and their out-of-sample forecasted values.
#   Please designate red color to represent the actual prices, 
#   and blue doted lines to represent forecasted values.
#   What do you think will happen to the price of each asset by the end of the year?
ts.plot(gold, goldforcos, lty = 1:2, col=c("red", "blue"))
# Gold prices remain relatively flat in the last 10 months of 2019 and ends slightly lower than Feb
ts.plot(oil, oilforcos, lty = 1:2, col=c("red", "blue"))
# Oil prices go up and down twice before finally settling down at a level above Feb prices
ts.plot(yen, yenforcos, lty = 1:2, col=c("red", "blue"))
# Yen prices go up and down and settle at roughly the same levels as in Feb

#30 Please calculate percentage between the price of each asset in 
#   February 2019 and their forecasted December 2019 prices. 
#   Which asset promises the highest rate of return? 

(goldforcos[10]-gold[170])*100/gold[170]
(oilforcos[10]-oil[170])*100/oil[170]
(yenforcos[10]-yen[170])*100/yen[170]

#Oil promises the highest return at 5.55%