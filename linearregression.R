
#set Working Directory 
setwd("E:/R/datasets")

#Load Dataset 
houseprices<-read.csv("Houseprices.csv",na.strings = c(""))
str(houseprices)

#Change  data types of columns 
col_names <- names(houseprices[-c(1,16)])
houseprices[,col_names] <- lapply(houseprices[,col_names ], factor)
str(houseprices)


 #Import Required Packages 
 install.packages(packagename)
 library(MASS)
 library(caret)
 library(car)

#Split data (Trianing and Testing)
set.seed(123)
housepartition<-createDataPartition(houseprices$Prices,p=0.7,list = FALSE)
houseprice_train<-houseprices[housepartition,]
houseprice_test<-houseprices[-housepartition,]

 #Build model with lm()-> function 
 model<- lm(houseprice_train$Prices ~ ., houseprice_train)
 summary(model)

#Prediction of data
pred<-predict(model,houseprice_test[-c(16)],type = "response")

pred

 # Error checking 
 error<-houseprice_test$Prices-pred
 square_error<-error**2
 square_root<- sqrt(mean(square_error))
 square_root

 #Checking variables siginficance 
 full<-lm(houseprice_train$Prices~.,houseprice_train)
 stepAIC(full,direction='backward')


