## Topics to be covered

# 1. Environment Overview
# 2. Installing and loading libraries
# 3. Datatypes
# 4. Objects Storage and Retrieval
# 5. Reading and writing external files
# 6. Data Manipulation
#         a. Conditional subsetting
#         b. Creating and removing variables
#         c. Sorting
#         d. Merging Datasets
#         e. Coercing datatypes
# 7. Functions




# 1. Environment Overview

# Hello World !!
cat("Hello World !")


## Working Directory

# know the present WD
getwd()

# Set the WD to a location of choice
setwd("C:\\R")
# Note the \\

# Create an object
a <- 4
a

b <- 5
g <- 9.4

# list objects in the environment
ls()

# NOTE 1 : R is case-sensitive
A

# NOTE 2 : R overwrites objects without warnings
a <- 6

# remove an object
rm(b)

# remove all objects
rm(list=ls())

# know the R version
R.Version()

# Clear console
# Ctrl + L



# 2. Installing and loading libraries

# Installing packages
install.packages("plyr")

# Loading packages for use
library(plyr)
# OR
require(plyr)


# Seeking help
help(plyr)
# OR
?plyr

# For keyword based search
??correlation
# OR
help.search('linear model')



# 3. Datatypes

# i. Numeric 
a <- 3.5

class(a)
mode(a)


# ii. Integer 
b <- as.integer(2)
class(b)

c <- 1:10
class(c)


# iii. Character
char_var <- "Hello"

class(char_var)

char_var1 <- c("A", "B", "C")


# iv. Logical
logical_var1 <- T
logical_var2 <- as.logical(c(2, 5, 0, -1))

logical_var3 <- (2 < 5)


class(logical_var1)


# v. Complex

complex_var1 <- 5 + 2i

complex_var2 <- 2 + 3i

complex_var1 * complex_var2

class(complex_var1)



# 4. Objects Storage and Retrieval

# I. Vector
A <- seq(1,100,by=5)

# Accessing specific elements
A[1]
A[5:7]
A[-1]

length(A)


# II. Matrix
test_matrix <- matrix(seq(1,100,by=5), nrow = 10, byrow = F)

class(test_matrix)
mode(test_matrix)

test_matrix[3, ]
test_matrix[, 2]
test_matrix[2,1]


# III. List
list_var <- list(name="Fred", mynumbers=A, mymatrix=test_matrix, age=5.3, Status = T)

class(list_var)
mode(list_var)

# Accessing elements by name
list_var$name
list_var$mynumbers[2]

# Accessing elements by Index
list_var[3]

list_var[[3]][3,2]


# IV. Data Frame
myDataFrame <- data.frame(test_matrix)

View(myDataFrame)

dim(myDataFrame)
nrow(myDataFrame)
ncol(myDataFrame)

names(myDataFrame)

names(myDataFrame) <- c("Column1", "Column2")

row.names(myDataFrame)
colnames(myDataFrame)

myDataFrame$Column1

myDataFrame[, 2]



# 5. Reading and writing external files

# Read data
iris_csv <- read.csv("iris.csv")

iris_tab <- read.table("iris_tab.txt", sep = "\t", header = T)

input_url <- read.csv(url("http://samplecsvs.s3.amazonaws.com/Sacramentorealestatetransactions.csv"))


# Write data
write.csv(iris_tab, "output_tab.csv", row.names = F)


# 6. Data Manipulation

#Variables or Lists
executive <- c(1, 2, 3, 4, 5)
date <- c("10/24/08",  "10/28/08",  "10/1/08",  "10/12/08",  "5/1/09")
country <- c("US",  "US",  "UK",  "UK",  "UK")
gender <- c("M",  "F",  "F",  "M",  "F")
age <- c(18,  45,  25,  39,  15)
u1 <- c(5,  3,  3,  3,  2)
u2 <- c(4,  5,  5,  3,  2)
u3 <- c(5,  2,  5,  4,  1)
u4 <- c(5, 5,  5,  NA,  2)
u5 <- c(5,  5,  2,  NA,  1)

#Dataframe
company <-data.frame(executive,
                     date,
                     country,
                     gender,
                     age,
                     u1,
                     u2,
                     u3,
                     u4,
                     u5,
                     stringsAsFactors=FALSE)

dim(company)
str(company)

# Identify Nulls
is.na(company)

# a. Conditional subsetting

company[1:3, ]

company[, c(1:4, 10)]

company[company$gender == "F", ]

var_list <- c("executive", "age", "country")

company[var_list]

company[!names(company) %in% var_list]


# b. Creating and removing variables

# Create a column
company$Agecat <- ifelse(company$age >= 18, "Adult", "Minor")

# Rename a column
names(company)[11] <- "categorical_age"

# Remove a column
company$categorical_age <- NULL


# c. Sorting

# Sort on column / vector
sort(company$age)

# Sort multiple columns
order(company$age)

sorted_data <- company[order(gender, -age), ]


# Faster technique for dataframes
library(plyr)

sorted_data1 <- arrange(company, desc(gender), age)


# d. Merging Datasets

# Appending datasets
# Row Binding
iris_combined <- rbind(iris_csv, iris_tab)

# Column Binding
ID <- 1:300
iris_combined1 <- cbind(iris_combined, ID)


dfA <- data.frame(ID = 1:10, Name = LETTERS[1:10], Age = rep(c(20, 30, 40, 50, 60), 2))

dfB <- data.frame(ID = c(1:5, 11), Dept = c("A", "B", "A", "B", "C", "A"))


# Using Merge
?merge
merged_inner <- merge(dfA, dfB, by = "ID", all = T)


# Using SQL commmands
install.packages("sqldf")
library(sqldf)

dfC <- sqldf("select A.*, B.Dept from dfA A inner join dfB B on A.ID = B.ID")


sqldf("select country, avg(age) from company group by country")


# e. Coercing datatypes
str(company)

is.character(company$country)

company$country_cat <- as.factor(company$country)

company$cat_date <- as.Date(company$date, format = "%m/%d/%y")

company$str_date <- as.character(company$cat_date)



# 7. Functions

# Define a function
var_stats <- function(x) {
          mean <- mean(x, na.rm = T)
          sd <- sd(x, na.rm = T)
          
          return(c(mean, sd))
}

# Call the function
var_stats(company$u4)



