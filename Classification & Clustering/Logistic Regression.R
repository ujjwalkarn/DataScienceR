#############################################################################

## Session: Logistic Regression in R 
## Created By: Ujjwal Karn           
## Date: 05-Oct-2015                 

#############################################################################

# Topics Covered
# 
# 1. Reading data and Summary Statistics
# 2. Outlier Detection
# 3. Missing Value Treatment
# 4. Correlation and VIF
# 5. Feature Selection Using IV 
# 6. Creating Training and validation Sets
# 7. Running the Logistic Model on Training Set
# 8. Evaluating Performance on Validation Set
#         a. ROC and AUC
#         b. Confusion Matrix
#         c. KS Statistic
# 9. Scoring the test data

##############################################################################
#Reading data and Summary Statistics

#change the working directory
setwd("C:\\Users\\ujjwal.karn\\Desktop\\Classification & Clustering")
     
train_data<-read.csv("data/train.csv")
test_data<-read.csv("data/test.csv")

#Summary Statistics
library(Hmisc)

head(train_data)
str(train_data)
summary(train_data)
describe(train_data)

head(test_data)
str(test_data)
summary(test_data)
describe(test_data)

# 2-way contingency tables
xtabs(~admit + prestige, data = train_data)

##############################################################################
# Outlier Detection

sapply(train_data[,1:3], function(x) quantile(x, c(.01,.05,.25,.5,.75,.90,.95, .99, 1),na.rm=TRUE) )

#gpa of 6.5 seems to be an outlier
train_data$gpa[train_data$gpa > 4] <- 4


##############################################################################
# Missing Value Imputation

sapply(train_data, function(x) sum(is.na(x)) )
train_data$gre[is.na(train_data$gre)] <- mean(train_data$gre, na.rm=TRUE)

train_data2<-train_data

sapply(train_data2, function(x) train_data2[,x][is.na(train_data2[,x])]<- mean(train_data2[,x], na.rm=TRUE))
##############################################################################
# Correlation and VIF 

cor(train_data[,1:3])

library(usdm)
vif(train_data[,1:3])

##############################################################################
# Information Value 

library(plyr)
library(sqldf)
library(rpart)

source("C:\\xyz.R")

file.sources = list.files("others", full.names=TRUE)
sapply(file.sources,source,.GlobalEnv)

data <- train_data
data$admit <- factor(data$admit, levels= c("1","0"))
levels(data$admit)

str(data)
iv.mult(data, y="admit", vars=c("gre","gpa","prestige"), summary="TRUE")


##############################################################################
# Create training and validation sets 

set.seed(123)
smp_size <- floor(0.7 * nrow(train_data))

train_ind <- sample(seq_len(nrow(train_data)), size = smp_size)

training <- train_data[train_ind, ]
validation <- train_data[-train_ind, ]

##############################################################################
# Running the Logistic Model on Training set 

?lm
?describe
?glm

admit ~ gre + gpa + prestige

mylogit <- glm(admit ~ gre + gpa + prestige, data = training, family = "binomial")

mylogit2 <- glm(admit ~ gpa + prestige, data = training, family = "binomial")

summary(mylogit2)
# See how prestige has been used as a dummy variable

confint(mylogit, level=.90)

# Caluclating Concordance
# Refer to the blog here to see  about Concordance
# http://shashiasrblog.blogspot.in/2014/02/binary-logistic-regression-fast.html

fastConc<-function(model){
  # Get all actual observations and their fitted values into a frame
  fitted<-data.frame(cbind(model$y,model$fitted.values))
  colnames(fitted)<-c('respvar','score')
  # Subset only ones
  ones<-fitted[fitted[,1]==1,]
  # Subset only zeros
  zeros<-fitted[fitted[,1]==0,]
  
  # Initialise all the values
  pairs_tested<-nrow(ones)*nrow(zeros)
  conc<-0
  disc<-0
  
  # Get the values in a for-loop
  for(i in 1:nrow(ones))
  {
    conc<-conc + sum(ones[i,"score"]>zeros[,"score"])
    disc<-disc + sum(ones[i,"score"]<zeros[,"score"])
  }
  # Calculate concordance, discordance and ties
  concordance<-conc/pairs_tested
  discordance<-disc/pairs_tested
  ties_perc<-(1-concordance-discordance)
  return(list("Concordance"=concordance,
              "Discordance"=discordance,
              "Tied"=ties_perc,
              "Pairs"=pairs_tested))
}


fastConc(mylogit)
##############################################################################
#Check Performance on the Validation Set 

val <-predict(mylogit, validation, type="response") 

mydf <-cbind(validation,val)

mydf$response <- as.factor(ifelse(mydf$val>0.5, 1, 0))


library(ROCR)
logit_scores <- prediction(predictions=mydf$val, labels=mydf$admit)


#PLOT ROC CURVE
logit_perf <- performance(logit_scores, "tpr", "fpr")
plot(logit_perf,col = "darkblue",lwd=2,xaxs="i",yaxs="i",tck=NA, main="ROC Curve")
box()
abline(0,1, lty = 300, col = "green")
grid(col="aquamarine")


### AREA UNDER THE CURVE
logit_auc <- performance(logit_scores, "auc")
as.numeric(logit_auc@y.values)  ##AUC Value


#CONFUSION MATRIX
library(caret)
confusionMatrix(mydf$response,mydf$admit)


### KS STATISTIC
logit_ks <- max(logit_perf@y.values[[1]]-logit_perf@x.values[[1]])
logit_ks


## LIFT CHART
lift.obj <- performance(logit_scores, measure="lift", x.measure="rpp")
plot(lift.obj,
     main="Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")


#GAINS TABLE
#install.packages("gains")
library(gains)
# gains table
gains.cross <- gains(actual=mydf$admit , predicted=mydf$val, groups=10)
print(gains.cross)

##############################################################################
#Scoring the Test Data using the model we just created 

pred <- predict(mylogit, test_data, type="response") 
final <- cbind(test_data,pred)

write.csv(final,"final_probs.csv")

##############################################################################
#REFERENCE MATERIAL

## http://www.ats.ucla.edu/stat/r/dae/logit.htm
## http://www.unc.edu/courses/2010fall/ecol/563/001/notes/lecture21%20Rcode.html
## Caret Package: http://topepo.github.io/caret/
## http://www.r-bloggers.com/gini-index-and-lorenz-curve-with-r/
