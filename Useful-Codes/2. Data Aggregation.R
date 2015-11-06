# Topic covered are
#  Data Aggregation
#       - Reshaping variables to obsrvations and back (reshape2 package)
#       -  using aggregate function & Sqldf 
#  Cross Tabulation (xtabs)
#  Recoding variables (within, ifelse functions)
#  Conditional Transformation (cut2 functions)
#  Missing values Identification & treatment
#  Renaming variables
#  Joining & merging data sets 
#  Dates and timeframes
#  Standardisation
#  Partitioning
#  Frequency Distribution (Deducer package)


library("HSAUR2")
#library(xlsx)
data<- Forbes2000
#Export to a csv or a xlsx file
# write.csv(data, file="data.csv",row.names = FALSE)
# write.xlsx(data, "data.xlsx",row.names = FALSE)


library(reshape2)
library("plyr")
m<-melt(data,id.vars=c("rank", "name","country", "category" ), measures= c("sales", " profits",
                                                                           "assets",  "marketvalue"))  # converting from wide to long
table (m$variable)

data_wide<- dcast(m,country + category ~ variable, sum) # wide & aggregated

# Example of aggregation  by filter
data_wide<- dcast(m,country + category ~ variable, sum, subset= .(variable =="sales" & country=="Africa"))

summary(data) #summarise data frame

# Calculate aggregate statistics
# Calculate numerical summaries for subsets of a data frame : 
#apply(data, 2, mean)

#find the sum of assets  based  on country
library("sqldf")
agg_data<- sqldf("select country,category, sum(assets) from data group by  country,category")

#using aggregate function
agg_data<- aggregate(data$assets ~ data$country , FUN=sum)

#find the sum of assets  based  on country
agg_data1<- aggregate(data$assets + data$marketvalue ~ data$country , FUN=sum)

#find the sum of assets  based  on country and category
agg_data2<- aggregate(data$assets ,by=list(country=data$country,category=data$category) , FUN=sum)

#limitation of the aggregate function is that it can be used with  functions that return a sungle value



#Cross Tabulation
test<-xtabs(assets + marketvalue ~ country + category, data = data)
test

#export  cross tab results to a csv file

write.csv(xtabs(assets + marketvalue ~ country + category, data = data), "test.csv")
xtabs

# recoding Variables using within function
?within
data  <-   within(data,{
  salescat <- NA
  salescat[sales > 11]  		 <- "High"
  salescat[sales >= 7.1 & sales <= 11] 	 <- "Medium"
  salescat[sales < 7.1] 			 <- "low" })

# recoding Variables using ifelse function
data$salescat1<- ifelse(data$sales >11 ,"High", ifelse (data$sales >=7.1 & data$sales <= 11,"Medium","low") )

#Conditional transformations

# Cutting Functions - 
library("Hmisc")
data$cut_test<-cut2(data$sales,c(7.1,11))
table(data$cut_test)#Creates a factor with default labels to indicate where the data got cut

cut_g<-cut2(data$sales,g=3) # g stands for number of groups
table(cut_g)

cut_m<-cut2(data$sales,m=1000) # m stands for minimum number of observations in each group
table(cut_m)

#missing values 
# In R :
#   Missing values are represented by the symbol NA (not available) . 
# Impossible values (for example, dividing by 0) are represented by the symbol NaN (not a number) 
# R uses the same missing values symbol for character and numeric data.
# The function is.na( ) allows you to test for the presence of missing values.

#Substitute the col mean with missing values

summary(data$profits)
data$profits_imp<-data$profits
data$profits_imp[which(is.na(data$profits_imp))]<-mean(data$profits_imp,na.rm=TRUE)
which(is.na(data1$profits))

#Function to get missing values in data
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      nmiss=sum(is.na(x)), 
      n=length(x), 
      propmiss=sum(is.na(x))/length(x)
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$propmiss), ])
}



# filter out variables with more than 10% missing values
data<-data1
list_var_missing <- propmiss(data) #LIST of variables with missing values
list_var_missing1<- list_var_missing[which(list_var_missing$propmiss > 0.0023),]
drop_miss_varlist<-list_var_missing1$variable
drop_miss_varlist
data<-data[,!names(data) %in% drop_miss_varlist]



#Renaming variables and observations

names(data1)
# To rename profits_imp to imp_profits
names(data1)[12]   <-   "imp_profits"


# joining and merging datasets
data<-Forbes2000
myleft<-data[c("rank", "name", "country")]
myright <-data[c("rank",  "category", "sales", "assets")]
#merge the two dataframes by rank and name
both<-merge(myleft,myright,by="rank")
both1<-merge(myleft,myright,by.x="rank",by.y="rank") #allowing the rank variables to have diffrent names in each datasets

#Left outer: merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)

#Right outer: merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)


#Dates and time frames
#Dates are represented as the number of days since 1970-01-01, with negative values for earlier dates.
# use as.Date( ) to convert strings to dates
mydates <- as.Date(c("2007-06-22", "2004-02-13"))# number of days between 6/22/07 and 2/13/04
days <- mydates[1] - mydates[2] 
days

Sys.Date( ) #returns today's date.
date() #returns the current date and time

# print today's date
today <- Sys.Date()
format(today, format="%B %d %Y") 

#Use as.Date()function to convert character data to dates. 
# convert date info in format 'mm/dd/yyyy'
strDates <- c("01/05/1965", "08/16/1975")
dates <- as.Date(strDates, "%m/%d/%Y")
dates

#The default format is yyyy-mm-dd
mydates <- as.Date(c("2007-06-22", "2004-02-13"))

#Date to Character
strDates <- as.character(dates) 



# other important functions
#standardisation
food <- read.table("http://www.stat.sc.edu/~hitchcock/foodstuffs.txt", header=T)
attach(food) 
# first scale the data by dividing each variable by its standard deviation: 
# finding standard deviations of variables 
std <- apply(food[,-1],2,sd)
std
food.std <- sweep(food[,-1],2,std,FUN="/")

#partitioning
set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
train <- iris[ind==1,]
test <- iris[ind==2,]


#proc frequency
install.packages("Deducer")
library("Deducer")
frequencies(data$category)
