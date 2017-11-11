setwd("C:\\Work\\BIG_DATA\\BankOfBaroda\\BoB_hackathon_workspace\\R_DT_Recommend")

library(queueing)
library(plyr)
library(lubridate)

library(C50)


test_data <- read.csv("DT_predict_this.csv", header = FALSE , sep = ",")
colnames(test_data)[1]  <- "ATM_NAME"
colnames(test_data)[2] <- "ATM_UP"
colnames(test_data)[3] <- "CASH"
colnames(test_data)[4] <- "DISTANCE"
colnames(test_data)[5] <- "TRAVEL_TIME"
colnames(test_data)[6] <- "WAITING_TIME"

str(test_data)

predict_Data_atm_name <- test_data[,1]
predict_Data <- test_data[,-1]
predictionModel <- readRDS("DecisionTreeTrain.rds")

result <- predict(predictionModel,predict_Data)

output <- cbind.data.frame(test_data,result)

names(output)
output_yes <- output[output$result =='Yes',]


order_output <- output_yes[order(output_yes$DISTANCE,output_yes$TRAVEL_TIME),]
order_output <- order_output[,-7]
order_output_x<- order_output[1:3,]
write.table( order_output_x,"DT_predict_output.csv", sep=",",  col.names=FALSE ,row.names = FALSE)
