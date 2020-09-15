#Chapter 4: Exploratory Data Analysis with R; Roger D. Peng

#install packages
library(readr)
library(dplyr)
library(tidyr)
#Read the file
ozone <- read_csv("US EPA data 2017.csv")

#Get rid of spaces in the names for future convenience analysis
names(ozone) <- make.names(names(ozone))

#Check the dataset for number of rows and number of columns
nrow(ozone)
ncol(ozone)


#check all the datatypes and sample data using str function
str(ozone)


#View the top few and bottom few lines of your data
head(ozone[, c(6:7, 10)])

tail(ozone[, c(6:7, 10)])

table(ozone$Date.of.Last.Change, ozone$State.Code)

#filter all measurements taken on Sep 17, 2018

filter(ozone, Date.of.Last.Change == "2018-09-19") %>%
  select(Date.of.Last.Change, Arithmetic.Mean) %>% 
  as.data.frame

#Check whether all states data is in the dataset
select(ozone, State.Name) %>% unique %>% nrow

#There cannot be 54 states in the US. So we check the data to see what is wrong

unique(ozone$State.Name)

#DC, Peurto Rico, Mexico, Virgin Islands are added to the list

#We can look at measurements statistical summary to get a sense of the dat

ozone1<-filter(ozone, ozone$Units.of.Measure == 'Parts per million')
summary(ozone1$Arithmetic.Mean)

#Quartiles of the data can be used to get a sense of what the data is like
quantile(ozone1$Arithmetic.Mean, seq(0, 1, 0.1))

#We can group the data by state and county and arrange in descending order of ozone PPM
ranking <- group_by(ozone1, State.Name, County.Name) %>%
           summarize(ozone = mean(Arithmetic.Mean)) %>%
           as.data.frame %>%
           arrange(desc(ozone))

#We can look at the top 10 counties
head(ranking, 10)

#We can look at bottom 10 coounties
tail(ranking, 10)


#We can look at the number of rows for the highest ozone which is Indiana, Bartholomew
filter(ozone, State.Name == "Indiana" & County.Name == "Bartholomew") %>% nrow

#Filter to the bottom most county
filter(ozone, State.Name == "Pennsylvania" & County.Name == "Warren") %>% nrow


# We can look at the mean monthly california ozone ppm
filter(ozone1, State.Name == "California") %>%
           mutate(month = factor(months(Date.of.Last.Change), levels = month.name)) %>%
           group_by(month) %>%
           summarize(ozone = mean(Arithmetic.Mean))


# We can sample a random sample of the data
set.seed(10234)
N <- nrow(ozone1)
idx <- sample(N, N, replace = TRUE)
ozone2 <- ozone[idx, ]

# We can check the rankings again with the sampled data
ranking2 <- group_by(ozone2, State.Name, County.Name) %>%
           summarize(ozone = mean(Arithmetic.Mean)) %>%
           as.data.frame %>%
           arrange(desc(ozone))

#Compare the top 10 of each dataset

cbind(head(ranking, 10), head(ranking2, 10))

#Compare the bottom 10 of each dataset
cbind(tail(ranking, 10), tail(ranking2, 10))
