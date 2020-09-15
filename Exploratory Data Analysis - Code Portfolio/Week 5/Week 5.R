#12 Tidy Data (R for Data Science, Garrett Grolemund, Hadley Wickham)

library(tidyverse)
library(ggplot2)
library(stringr)

# The below tables shows the same data organized in a different way
table1

table2

table3

table4a

table4b

#Table 1 is tidy with each variable having a column and each row being an observation 

#Calculate the rate using the data in table1
table1 %>% 
  mutate(rate = cases / population * 10000)

#calculate cases per year
table1 %>% 
  count(year, wt = cases)

#Study the changes in the data over time in a plot
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

#gather() is used when we find situations where columns are variable values

table4a

table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")

table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")

# we can combine both tables together using left_join function

tidy4a <- table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b)

#Spread() is used when an observation is scattered across multiple rows

table2
  
table2 %>%
  spread(key = type, value = count)

stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks

stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`,`2016`)

table4a %>% 
  gather(`1999`,`2000`, key = "year", value = "cases")

people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

people

#separate() is used to split one column into multiple column based on separators

table3

table3 %>% 
  separate(rate, into = c("cases", "population"))

# By default it takes whatever non-alphanumeric is used as separator. We can also explicitly specify it as shown below
table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")

#As can be seen the new columns are stored as character. In order to convert it, we need to add the convert option to the function

table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)

#separate() is used to add positions of integers to split at
table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)

#unite() combines columns and works in the opposite way of separate()
table5 %>% 
  unite(new, century, year)

#It inserts "_" by default. We can manually specify the seperators
table5 %>% 
  unite(new, century, year, sep="")


stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks

#2016 Qtr1 is missing because it does not have an entry. It is implicit. We can mmake it explicity by spreading the columns
stocks %>% 
  spread(year, return)

#To reverse it, we can exclude implicit values
stocks %>% 
  spread(year, return) %>% 
  gather(year, return, `2015`:`2016`, na.rm = TRUE)

stocks

#complete() can be used to make the missing data more explicit
stocks %>% 
  complete(year, qtr)

treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)
treatment

#fill() replaces missing values with previous one
treatment %>% 
  fill(person)

#Case Study

who<-read.csv("C:/Users/subha/Desktop/ANLY506/ANLY506/TB_notifications_2019-07-08.csv")

who<-as_tibble(who)
who

# we take columns from m014 onwards into one column as those are variable values not column names

who1 <- who %>% 
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE)
who1

#calculate total cases by key
who1 %>% 
  count(key)


#first step is to rename newrel columns to new_rel for easy separation in subsequent steps

who2 <- who1 %>% 
  mutate(key = stringr::str_replace(key, 'newrel', 'new_rel'))

#there is one additional column in the new data that also needs to be renamed
who2 <- who2 %>% 
  mutate(key = stringr::str_replace(key, 'rel_in_agesex_flg', 'new_rel_inagesexflg'))

#Just checking to make sure that the renaming has been successfully achieved
table(who2$key)

#separate the key column into three columns at "_"
who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")

#check to make sure there is only one value for new column so that it can be dropped
who3 %>% 
  count(new)

#drop the new column and iso2 and iso3
who4 <- who3 %>% 
  select(-new, -iso2, -iso3)

#separate sexage column into two new columns-sex and age
who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1)

# all of this can be combined into one statement
who %>%
  gather(key, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  mutate(key = stringr::str_replace(key, 'rel_in_agesex_flg', 'new_rel_inagesexflg')) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

