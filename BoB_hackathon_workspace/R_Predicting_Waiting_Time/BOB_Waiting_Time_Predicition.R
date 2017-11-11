#
setwd("C:\\Work\\BIG_DATA\\BankOfBaroda\\BoB_hackathon_workspace\\R_Predicting_Waiting_Time")


## import the packages required 
library(queueing)
library(plyr)
library(lubridate)


Wait_Queue_function <- function(input_lambda)
{
  temp <- summary(QueueingModel(NewInput.MM1(lambda=input_lambda, mu=60, n=1)))
  y <- temp$el$Wq
}



## checking this


#MM1_model   <- NewInput.MM1(lambda=20, mu=60, n=1)
#o_mm1kk <- QueueingModel(MM1_model)
#saveRDS(o_mm1kk ,"queue_model")

atm_data <- read.csv("C:\\Work\\BIG_DATA\\BankOfBaroda\\BoB_hackathon_workspace\\R_Predicting_Lambda\\predicited_lambda.csv",header = FALSE,sep = ",")

colnames(atm_data)[1] <- "ATM_NAME"
colnames(atm_data)[2] <- "DATE" 
colnames(atm_data)[3] <-  "DAY"
colnames(atm_data)[4] <-  "HOUR"
colnames(atm_data)[5] <-  "YEAR"   
colnames(atm_data)[6] <-  "WEEK_OF_YEAR"     
colnames(atm_data)[7] <- "MONTH"
colnames(atm_data)[8] <- "WEEK_OF_MONTH"
colnames(atm_data)[9] <- "DAY_OF_WEEK"
colnames(atm_data)[10] <- "predicted_lambda"
  

atm_data_wq <- cbind(atm_data , Wait_Queue_function(atm_data$predicted_lambda)) 
colnames(atm_data_wq)[11] <- "WAIT_IN_HRS"


str(atm_data_wq)

###we have to give the prediction in seconds


atm_data_wq_sec <- cbind(atm_data_wq, atm_data_wq$WAIT_IN_HRS*60 * 60)

str(atm_data_wq_sec)
atm_data_wq_sec_1 <- atm_data_wq_sec[ ,c(1,12)]
colnames(atm_data_wq_sec_1)[2]  <- "WT_in_secs"

#write.csv(atm_data_wq_sec_1, file = "predicited_wait_time.csv" ,row.names=FALSE ,col.names = FALSE)
write.table( atm_data_wq_sec_1,"predicited_wait_time.csv", ,col.names=FALSE ,row.names = FALSE)
