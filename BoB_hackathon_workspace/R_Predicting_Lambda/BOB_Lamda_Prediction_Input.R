## set the working directory



setwd("C:\\Work\\BIG_DATA\\BankOfBaroda\\BoB_hackathon_workspace\\R_Predicting_Lambda")
# load the model


library(queueing)
library(plyr)
library(lubridate)

predictionModel <- readRDS("lambda_prediction.rds")

## preparing the  test data as required by the model 
test <- read.csv("Predict_Lambda_for_this.csv", header = FALSE , sep ="," ) 

colnames(test)[1] <- "ATM_NAME"
colnames(test)[2] <- "DATE"
colnames(test)[3] <- "DAY"
colnames(test)[4] <- "HOUR"

str(test)


test$ATM_NAME <- as.factor(test$ATM_NAME)

test <- cbind( test , substr(test$DATE , 0 ,4))
colnames(test)[5] <-"YEAR"


str(test)

#test$HOUR <-  factor(test$HOUR)
## week of the year 
mydf <- as.Date(test$DATE, format="%Y/%m/%d")
WEEK_OF_YEAR <- as.numeric( format(mydf+3, "%U"))
test <- cbind(test,WEEK_OF_YEAR)
##Month
test <- cbind( test ,month(as.POSIXlt(test$DATE, format="%Y/%m/%d")))
colnames(test)[7] <- "MONTH"
## get the week of the month 
## function that calculate the week of the month 
first_day_of_month_wday <- function(dx) {
  day(dx) <- 1
  wday(dx)
}
test <- cbind.data.frame(test ,ceiling((day(as.POSIXlt(test$DATE, format="%Y/%m/%d")) + first_day_of_month_wday(as.POSIXlt(test$DATE, format="%Y/%m/%d")) - 1) / 7) )
colnames(test)[8] <- "WEEK_OF_MONTH"

## DAY of the week 
test <- cbind.data.frame(test , (as.POSIXlt(test$DATE)$wday + 1)  )
colnames(test)[9] <- "DAY_OF_WEEK"
## hour of the day 
library(lubridate)
#test <- cbind(test,format(strptime(test$ST_DATE,"%H:%M:%S"),'%H'))
#colnames(test)[10] <- "HOUR"
#str(test)

#ATM_NAME + DAY + WEEK_OF_MONTH + HOUR
predicted_lambda  <- predict.lm(predictionModel,test)

final_predicition <- cbind(test,predicted_lambda)
## write the output of the prediction to the File
##

#write.csv(final_predicition, file = 
write.table(final_predicition,"predicited_lambda.csv",row.names=FALSE,col.names = FALSE,sep=",")

