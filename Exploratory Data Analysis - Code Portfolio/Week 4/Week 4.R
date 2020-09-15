#Chapter 8 Matrices and Dataframes (YaRrr! The Pirate's Guide to R, Nathaniel D. Phillips)

x <- 1:5
y <- 6:10
z <- 11:15

# Create a matrix where x, y and z are columns
cbind(x, y, z)

# Create a matrix where x, y and z are rows
rbind(x, y, z)


# Creating a matrix with numeric and character columns will make everything a character:

cbind(c(1, 2, 3, 4, 5),
      c("a", "b", "c", "d", "e"))

# Create a matrix of the integers 1:10,
#  with 5 rows and 2 columns

matrix(data = 1:10,
       nrow = 5,
       ncol = 2)

# Now with 2 rows and 5 columns
matrix(data = 1:10,
       nrow = 2,
       ncol = 5)

# Now with 2 rows and 5 columns, but fill by row instead of columns
matrix(data = 1:10,
       nrow = 2,
       ncol = 5,
       byrow = TRUE)


# Create a dataframe of survey data

survey <- data.frame("index" = c(1, 2, 3, 4, 5),
                     "sex" = c("m", "m", "m", "f", "f"),
                     "age" = c(99, 46, 23, 54, 23))

str(survey)

# Create a dataframe of survey data WITHOUT factors
survey <- data.frame("index" = c(1, 2, 3, 4, 5),
                     "sex" = c("m", "m", "m", "f", "f"),
                     "age" = c(99, 46, 23, 54, 23),
                     stringsAsFactors = FALSE)
str(survey)


data("ChickWeight")

# head() to get first few rows
head(ChickWeight)

# tail() to get last few rows
tail(ChickWeight)

# View() to see the entire dataset
View(ChickWeight)

data("ToothGrowth")

# Summary statistical data 
summary(ToothGrowth)

# print datatype info for each variable
str(ToothGrowth)

# names of columns
names(ToothGrowth)

# view column named len
ToothGrowth$len

# mean of the len column
mean(ToothGrowth$len)

# table of the column supp returns distinct values and count
table(ToothGrowth$supp)

# print first fiew rows of specific columns
head(ToothGrowth[c("len", "supp")])

# Create dataframe named survey
survey <- data.frame("index" = c(1, 2, 3, 4, 5),
                     "age" = c(24, 25, 42, 56, 22))

# Create a new column with sex
survey$sex <- c("m", "m", "f", "f", "m")

survey

# Change name of columns

names(survey)[1] <- "participant.number"

#To avoid entering column number use names to refer to the column

names(survey)[names(survey) == "age"] <- "years"


#Slicing dataframes

#Return row 1-6 and column 1 of ToothGrowth
ToothGrowth[1:6, 1]

#Row 1-3 and columns 1 and 3
ToothGrowth[1:3, c(1,3)]

#1st row and all columns
ToothGrowth[1, ]

#2nd column and all rows
ToothGrowth[, 2]


#Create a new dataframe where one column has a specific value
ToothGrowth.VC <- ToothGrowth[ToothGrowth$supp == "VC", ]

#multiple conditions
ToothGrowth.OJ.a <- ToothGrowth[ToothGrowth$supp == "OJ" &
                                  ToothGrowth$dose < 1, ]

#use subset() function to achieve the same in an easier manner
subset(x = ToothGrowth,
       subset = len < 20 &
         supp == "OJ" &
         dose >= 1)

#add select in the subset() command to select specific columns
subset(x = ToothGrowth,
       subset = len > 30 & supp == "VC",
       select = c(len, dose))

#combine slicing with other functions

#subset data with OJ
oj <- subset(x = ToothGrowth,
             subset = supp == "OJ")

#calculate mean of len column in this subset data
mean(oj$len)

# can achieve the same by directly filtering and calculating mean
mean(ToothGrowth$len[ToothGrowth$supp == "OJ"])

#with() helps save time by using it once to refer to the same dataframe for all calculations

health <- data.frame("age" = c(32, 24, 43, 19, 43),
                     "height" = c(1.75, 1.65, 1.50, 1.92, 1.80),
                     "weight" = c(70, 65, 62, 79, 85))

health

#One way
health$weight / health$height ^ 2

#other way
with(health, weight / height ^ 2)

health$weight + health$height / health$age + 2 * health$height

with(health, weight + height / age + 2 * height)


#20 Vectors (R for Data Science, Garrett Grolemund, Hadley Wickham)

library(tidyverse)

#typeof() is used to determine the datattype of vectors
typeof(letters)

typeof(1:10)

#length is used to determine the number of datapoints in each vector
x<-list("a", "b", 1:10)
length(x)
# the length is 3 because if has 3 datapoints

#logical vectors can take only one of three forms: TRUE, FALSE, NA depending on whether they meet certain conditions

1:10%%3==0 # This generates a logical vector with TRUE or FALSE depending on whether the reminder when dividing 1 to 10 by3 is 0 or not
c(TRUE, TRUE, FALSE, NA)

#integer and double (floating point data) are collectively called numeric data

typeof(1)

typeof(1L)
#because numbers are double by default, it is important to add L at the end to recognize it as an integer

#doubles are approximations, example below
x<-sqrt(2)^2
x
x-2
#here x-2 is not 0 just very very close to 0. because x is stored as double not integer

#Integers have just 1 special value i.e. NA, but double has 4 special values: NA, NaN, -Inf, +Inf

c(-1,0,1)/0

#character vector
#each unique string vector is stored once in memory and duplicates are just pointers pointing to the same location in memory

x<- "This is a reasonably long string."
install.packages("pryr")
library(pryr)

object_size(x)

y<-rep(x,1000)

object_size(y)

x <- sample(20, 100, replace = TRUE)
y <- x > 10
sum(y)
mean(y)

#R implicitly coerces the length of vectors. When two vectors are combined, the shorter vector is recycled to the same length as longer vector

sample(10) + 100

runif(10) > 0.5

1:10 + 1:2
# This does not happen to the combination below because a vector of length 3 cannot be duplicated to a vector of length 10
1:10 + 1:3


tibble(x = 1:4, y = 1:2)
tibble(x = 1:4, y = rep(1:2, 2))
tibble(x = 1:4, y = rep(1:2, each = 2))

c(x = 1, y = 2, z = 4)

set_names(1:3, c("a", "b", "c"))

x <- c("one", "two", "three", "four", "five")
#subsetting can be achieved by selecting the specific columns
x[c(3, 2, 5)]

x[c(1, 1, 5, 5, 5, 2)]
#using '-' removes the specific columns
x[c(-1, -3, -5)]

#positives and negatives cannot be combined
x[c(1, -1)]

x <- c(10, 3, NA, 5, 8, 1, NA)

# to subset all non NA values
x[!is.na(x)]

# to subset all even or missing values
x[x %% 2 == 0]

x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]

x <- list(1, 2, 3)
x
#A list contains other lists.The above list has three elements each of which is a list with one element

#datatype
str(x)

x_named <- list(a = 1, b = 2, c = 3)
str(x_named)

y <- list("a", 1L, 1.5, TRUE)
str(y)
#A list can have multiple datatypes within the list

#lists can be made of lists
z <- list(list(1, 2), list(3, 4))
str(z)

x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))


a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
str(a[1:2])
# Datatype of first two datapoints in the list
str(a[4])
#Datatype of 4th datapoint in the list

str(a[[1]])
str(a[[4]])
#$ can be used similar to [[]]
a$a
a[["a"]]


#Attributes are additional metadata attached to the vectors

x<-1:10
x
attr(x,"greeting")
attr(x,"greeting")<-"Hi!"
attr(x,"farewell")<-"Bye!"
attributes(x)

x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
x
typeof(x)
attributes(x)

x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
attributes(x)

x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)
typeof(x)
attributes(x)


x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)
typeof(x)
attributes(x)
attr(x, "tzone") <- "US/Pacific"
x
attr(x, "tzone") <- "US/Eastern"
x
y <- as.POSIXlt(x)
typeof(y)
attributes(y)

tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb)
attributes(tb)

df <- data.frame(x = 1:5, y = 5:1)
df
typeof(df)
attributes(df)


