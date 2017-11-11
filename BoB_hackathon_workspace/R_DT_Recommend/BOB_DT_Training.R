setwd("C:\\Work\\BIG_DATA\\BankOfBaroda\\BoB_hackathon_workspace\\R_DT_Recommend")

## this is for Lambda predcition



## import the packages required 
library(queueing)
library(plyr)
library(lubridate)
library(C50)


data <- read.csv("DT_training.csv", header = FALSE , sep = ",")
colnames(data)[1] <- "ATM_UP"
colnames(data)[2] <- "CASH"
colnames(data)[3] <- "DISTANCE"
colnames(data)[4] <- "TRAVEL_TIME"
colnames(data)[5] <- "WAITING_TIME"
colnames(data)[6] <- "recommend"

## tree methods are available in this method 

require(C50)

#train_me<- data[1:10, -6]
#test_me <- data[11:14,-6]
data_train <- data[, -6]
train_target <- data[,6]
#test_target <- data[11:14,6]

model1 <- C50::C5.0(data_train,train_target)


saveRDS(model1, "DecisionTreeTrain.rds")

