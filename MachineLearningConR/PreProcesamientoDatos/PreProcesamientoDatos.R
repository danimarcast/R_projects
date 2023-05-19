library(readr)
library(MVN)
dataset<-read.csv("datasets/Data.csv")
x <- dataset[,-4]
y <- dataset[,4]
dataset$Age <-ifelse(is.na(dataset$Age),ave(dataset$Age, FUN = function(x) mean(x,na.rm = TRUE)),dataset$Age)  
  
dataset$Salary <-ifelse(is.na(dataset$Salary),ave(dataset$Salary, FUN = function(x) mean(x,na.rm = TRUE)),dataset$Salary)  
  