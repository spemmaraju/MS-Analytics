#Chapter 9 Importing, saving and managing data (YaRrr! The Pirate's Guide to R, Nathaniel D. Phillips)

#Get working directory on your system
getwd()

#Set working directory to your code portfolio folder
setwd("C:/Users/subha/Desktop/ANLY506/ANLY506")

#List of objects in current workspace that you have set
ls()

#Create some sample dataframes for reference
study1.df <- data.frame(id = 1:5, 
                        sex = c("m", "m", "f", "f", "m"), 
                        score = c(51, 20, 67, 52, 42))

score.by.sex <- aggregate(score ~ sex, 
                          FUN = mean, 
                          data = study1.df)

study1.htest <- t.test(score ~ sex, 
                       data = study1.df)

# The new objects now show up in the list when you use ls() function
ls()

#Save the objects in the working directory that you created
save(study1.df, score.by.sex, study1.htest,
     file = "study1.RData")

#If you want to save all the objects in the workspace use the following command
save.image(file = "projectimage.RData")

#load objects in the datafile into workspace
load(file = "study1.RData")

#load objects in image file into the workspace
load(file = "projectimage.RData")

#Remove specific objects
rm(score.by.sex)

#Remove all objects
rm(list=ls())

#load objects in the datafile into workspace
load(file = "study1.RData")

#Write data to text file
write.table(x = study1.df,
            file = "study1.txt",  # Save the file as study1.txt
            sep = "\t")            # Make the columns tab-delimited

#Remove all objects
rm(list=ls())

#Read table from txt file
mydata <- read.table(file = 'study1.txt',    # file location from working directory
                     sep = '\t',                  # file is tab--delimited
                     header = TRUE,               # This piece of code states that there is a header to the dataset
                     stringsAsFactors = FALSE)    # Very imoortant to not convert strings to factors

mydata
str(mydata)

#Read directly from web
fromweb <- read.table(file = 'http://goo.gl/jTNf6P',
                      sep = '\t',
                      header = TRUE)
fromweb


# Practice
rm(list=ls())
#4
a <- data.frame("sex" = c("m", "f", "m"),
                "age" = c(19, 43, 25),
                "favorite.movie" = c("Moon", "The Goonies", "Spice World"))
b <- mean(a$age)

c <- table(a$sex)

ls()

#Read directly from web
club.df <- read.table(file = 'http://nathanieldphillips.com/wp-content/uploads/2015/12/club.txt',
                      sep = '\t',
                      header = TRUE)

#Write data to text file
write.table(x = club.df,
            file = "club.txt",  # Save the file as txt
            sep = "\t")            # Make the columns tab-delimited

save.image(file = "myobjects.RData")

rm(list=ls())
