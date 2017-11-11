## set the working directory

setwd("C:\\Work\\BIG_DATA\\BankOfBaroda\\BoB_hackathon_workspace")

## this is for Lambda predcition



## import the packages required 
library(queueing)
library(plyr)
library(lubridate)


## read the Training data


train <- read.csv("output_2017.csv", header = FALSE , sep =",") 

colnames(train)[1] <- "ATM_NAME"
colnames(train)[2] <- "DATE"
colnames(train)[3] <- "DAY"
colnames(train)[4] <- "ST_DATE"
colnames(train)[5] <- "END_DATE"


train <- cbind( train , substr(train$DATE , 0 ,4))
colnames(train)[6] <-"YEAR"

## week of the year 
mydf <- as.Date(train$DATE, format="%Y/%m/%d")
WEEK_OF_YEAR <- as.numeric( format(mydf+3, "%U"))
train <- cbind(train,WEEK_OF_YEAR)


##Month
train <- cbind( train ,month(as.POSIXlt(train$DATE, format="%Y/%m/%d")))
colnames(train)[8] <- "MONTH"


## get the week of the month 
## function that calculate the week of the month 
first_day_of_month_wday <- function(dx) {
  day(dx) <- 1
  wday(dx)
}
train <- cbind.data.frame(train ,ceiling((day(as.POSIXlt(train$DATE, format="%Y/%m/%d")) + first_day_of_month_wday(as.POSIXlt(train$DATE, format="%Y/%m/%d")) - 1) / 7) )
colnames(train)[9] <- "WEEK_OF_MONTH"


## DAY of the week 


train <- cbind.data.frame(train , (as.POSIXlt(train$DATE)$wday + 1)  )

colnames(train)[10] <- "DAY_OF_WEEK"

## hour of the day 

library(lubridate)
train <- cbind(train,format(strptime(train$ST_DATE,"%H:%M:%S"),'%H'))
colnames(train)[11] <- "HOUR"

## create "1" as an event so that we can take the mean /per hour for each hour 

train <- cbind( train , 1)
colnames(train)[12] <- "EVENT"




## let us see for the one ATM 

#input_atm011 <- train[train$ATM_NAME == "ATM001"  ,]
#input_atm02 <- train[train$ATM_NAME == "ATM002"  ,]
#input_atm01 <- rbind(input_atm011 ,input_atm02)


train_data <- train[ ,c(1,3,9,10 ,11,12)]

str(train_data)

#atm_aggr <- aggregate( train_data$EVENT , list(train_data$ATM_NAME,train_data$WEEK_OF_MONTH,train_data$DAY_OF_WEEK,train_data$HOUR) ,sum)


atm_mean_day <- aggregate(train_data$EVENT , list(train_data$ATM_NAME,train_data$DAY,train_data$WEEK_OF_MONTH,train_data$HOUR) ,sum)
colnames(atm_mean_day)[1] <- "ATM_NAME"
colnames(atm_mean_day)[2] <- "DAY"
colnames(atm_mean_day)[3] <- "WEEK_OF_MONTH"
colnames(atm_mean_day)[4] <- "HOUR"
colnames(atm_mean_day)[5] <- "Walkins"
#aggregate(as.matrix(df[,5]),as.list(df[,1:4]),FUN = sum)

## preapare a Simple LM 
predictionModel <- lm(Walkins ~ DAY + WEEK_OF_MONTH + HOUR , data = atm_mean_day)

saveRDS(predictionModel, "./final_model.rds")


test <- read.csv("test_2017.csv", header = FALSE , sep =",") 

colnames(test)[1] <- "ATM_NAME"
colnames(test)[2] <- "DATE"
colnames(test)[3] <- "DAY"
colnames(test)[4] <- "ST_DATE"
colnames(test)[5] <- "END_DATE"


test <- cbind( test , substr(test$DATE , 0 ,4))
colnames(test)[6] <-"YEAR"

## week of the year 
mydf <- as.Date(test$DATE, format="%Y/%m/%d")
WEEK_OF_YEAR <- as.numeric( format(mydf+3, "%U"))
test <- cbind(test,WEEK_OF_YEAR)


##Month
test <- cbind( test ,month(as.POSIXlt(test$DATE, format="%Y/%m/%d")))
colnames(test)[8] <- "MONTH"


## get the week of the month 
## function that calculate the week of the month 
first_day_of_month_wday <- function(dx) {
  day(dx) <- 1
  wday(dx)
}
test <- cbind.data.frame(test ,ceiling((day(as.POSIXlt(test$DATE, format="%Y/%m/%d")) + first_day_of_month_wday(as.POSIXlt(test$DATE, format="%Y/%m/%d")) - 1) / 7) )
colnames(test)[9] <- "WEEK_OF_MONTH"


## DAY of the week 


test <- cbind.data.frame(test , (as.POSIXlt(test$DATE)$wday + 1)  )

colnames(test)[10] <- "DAY_OF_WEEK"

## hour of the day 

library(lubridate)
test <- cbind(test,format(strptime(test$ST_DATE,"%H:%M:%S"),'%H'))
colnames(test)[11] <- "HOUR"

str(test)

my_predictions  <- predict.lm(predictionModel,test)

final_predicition <- cbind(test[ ,test$ATM_NAME],my_predictions)




## write the output of the prediction to the File



write.csv(final_predicition, file = "predicited_lambda.csv")