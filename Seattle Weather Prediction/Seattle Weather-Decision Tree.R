# Load required libraries
library(tidyverse)
library(dplyr)
library(caret)
library(ISLR)
library(rpart)
library(rpart.plot)

#quick look at the data
head(mydata)
#select the necessary columns
mydata<- mydata %>%
  select(precipitation, temp_max, temp_min, wind, weather)
head(mydata)


set.seed(2000)
train.index=sample(1:dim(mydata)[1],dim(mydata)[1]*0.8)
train.df=mydata[train.index,]
valid.df=mydata[-train.index,]

default.ct=rpart(weather~.,data=train.df,method="class",control=rpart.control(maxdepth=4))
rpart.plot(default.ct,type=1,extra=2,under=TRUE,split.font=1,varlen=-5)

#Predict using first model
default.ct=as.factor(predict(default.ct,valid.df,type="class"))
valid.df.Loan=as.factor(valid.df$weather)
confusionMatrix(default.ct,valid.df.Loan)

# deeper tree
deeper.ct=rpart(weather~.,data=train.df,method="class",cp=0,minsplit=1)
length(deeper.ct$frame$var[deeper.ct$frame$var=="<leaf>"])

prp(deeper.ct,type=1,extra=1,under=TRUE,split.font=1,varlen=-5,box.col=ifelse(deeper.ct$frame$var=="<leaf>",'blue','red'))

#Prediction using test data
deeper.ct.pred.valid=as.factor(predict(deeper.ct,valid.df,type="class"))
confusionMatrix(deeper.ct.pred.valid,valid.df.Loan)

#The simpler model had an accuracy of 86.35% and it went down to 74.74% in the deeper tree model.
#From this example we can understand that making a model more complex does not make it better.





