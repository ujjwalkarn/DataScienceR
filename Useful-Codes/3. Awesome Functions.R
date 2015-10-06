# Get same sample always
set.seed(234);x <- rnorm(100)

plot(x)
hist(x)

# Cutting a vector into equi-length gourps
summary(cut(x,5))

# cutting a vector into groups of equal frequency
library(Hmisc)
summary(cut2(x,g = 5))

# Missing value imputation

x<- matrix(1:10, ncol=2)
x[c(1,3,7)] <- NA
print(x)
print(impute(x))
print(impute(x),fun=median)

# details of a dataset
describe(iris)

# functions / statistics based on categorical variables (one variable)
library(MASS)
data(birthwt)

bystats(birthwt$bwt,birthwt$race)
bystats(birthwt$bwt,birthwt$race,
              fun = function(x) c(Mean=mean(x),Median=median(x),StDev=sd(x)))
bystats(birthwt$bwt,birthwt$race, fun = quantile)

# functions / statistics based on categorical variables (more than one variables)
summary.formula(bwt ~ lwt + ht + race, data = birthwt,fun = mean)


# outlier capping
library(scales)
x <- c(runif(50,0,1),3); set.seed(24)
squish(x,quantile(x,probs=c(0.01,0.99)))

# Add new column
library(plyr)
new <- mutate(iris,new_col_1 = Sepal.Width*10)

# Applying function based on each desired subsets of a table
new <- ddply(iris, "Species", function(x) mean(x[,1]))
new <- ddply(iris, "Species", summarise,
                      N    = length(Sepal.Width),
                      mean = mean(Sepal.Width),
                      sd   = sd(Sepal.Width),
                      se   = sd / sqrt(N)
                      )

# Sort by rows
arrange(iris,Sepal.Width,desc(Sepal.Length))

# Renaming a column
rename(iris2 , c("Sepal.Width" = "SepWid"))

# Count of observations for each level of a categorical variable
count(iris,"Species")


library(dplyr)

# Sampling from a dataset by specifying proportion
sample_frac(iris, 0.1, replace = TRUE)

# Sampling from a dataset by specifying number of rows
sample_n(iris, 15, replace = TRUE)

# Extracting columns based on partial information
select(iris,contains("Sepal"))
select(iris,ends_with("Length"))
select(iris,starts_with("Petal"))

# Extracting a range of columns
select(iris, Sepal.Length:Petal.Width)

# Excluding a column
select(iris, -Species)

# Series of data manipulation steps

iris %>% 
  group_by(Species) %>%
  summarise(avgmpg = mean(Sepal.Length), avgwt = mean(Petal.Length))

# Filtering observations

filter(iris,Sepal.Width >2.7)
filter(iris,Sepal.Width >2.7, Petal.Width<1.9)
filter(iris,Sepal.Width >2.2, Petal.Width<0.5, Species == "setosa")


library(usdm)

# Checking VIF for a set of independent variables in a dataset
vif(LifeCycleSavings)
usdm::vif(LifeCycleSavings)

library(lubridate)

# Date and time manipulation
bday <- dmy("23121984")
bday

year(bday)
week(bday)
wday(bday)
wday(bday, label=TRUE)
yday(bday)

# Creating date and time
years(1)
months(1)
days(1)
hours(1)
minutes(1)
seconds(1)

x <- ymd("2012-03-26")
week(x)

# Time / date conversion
tm1.lub <- ymd_hms("2013-07-24 23:55:26")
tm1.lub
tm2.lub <- mdy_hm("07/25/13 08:32")
tm2.lub
tm3.lub <- ydm_hm("2013-25-07 4:00am")
tm3.lub
tm4.lub <- dmy("26072013")
tm4.lub
hour(tm2.lub)

# Current date and time
today()
now()

# Basic plotting
plot(cars)

# Specify color
plot(cars, col = "red")                  

# Specify connector-type
plot(cars, col = "red", type="l")       
plot(cars, col = "red", type="p")

# Add line to an existing plot
data(faithful)
fit <- lm(waiting~eruptions, data=faithful)
plot(faithful)
lines(faithful$eruptions, fitted(fit), col="blue")
abline(h=mean(faithful$waiting))
abline(fit, col = "red")

# Overlaid graphs
set.seed(213)                           # Get same sample always
samp <- sort(sample(1:1000,100))
plot(samp)
predict_samp <- samp+rnorm(100,5,10)
plot(predict_samp)
matplot( 1:100 , cbind(samp,predict_samp), col = c("black","red"), pch =16)

# Plot Histogram
hist(airquality$Ozone)
hist(airquality$Ozone,main="Distribution of w1",xlab="w1")
hist(airquality$Ozone,main="Distribution of w1",xlab="w1", breaks = 15)
hist(airquality$Ozone, main='Amoiunt of Ozone in Air', xlab='Ozone Content')
boxplot(airquality$Ozone)

# Outlier detection
airquality[!airquality$Wind %in% boxplot.stats(airquality$Wind)$out,]
airquality[airquality$Wind %in% boxplot.stats(airquality$Wind)$out,]

# Multiple graphs on same window
par(mfrow=c(1,2))
hist(cars[,1])
hist(cars[,2])

dev.off()

# Make column graph
counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution", xlab="Number of Gears")

# Make horizontal bar diagram
counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution", xlab="Number of Gears")

# Specifying axis scale
barplot(counts, main="Car Distribution", xlab="Number of Gears", xlim = c(0,16), horiz = TRUE)


library(ggplot2)

c <- ggplot(mtcars, aes(factor(cyl)))
c + geom_bar()
c + geom_bar(width=.5)
c + geom_bar(fill="skyblue")

qplot(factor(cyl), data=mtcars, geom="bar")
qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(cyl))

library(mice)

# Missing data pattern in a dataset
md.pattern(airquality)

# Missing value imputation
tempData <- mice(airquality,m=5,maxit=50,meth='mean')

# Complete the rows after imputation
completedData <- complete(tempData,1)

# One-step approach to get row and column statistics

colSums(iris[,-5])
rowMeans(iris[,-5])
rowSums(iris[,-5])
rowMeans(iris[,-5])

cumsum(iris[,-5])
cumprod(iris[,-5])
cummax(iris[,-5])
cummin(iris[,-5])

# Create dummy variables

library(dummies)
month.name
dummy(month.name)
