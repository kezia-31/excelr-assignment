data=read.csv(file.choose())
attach(data)
View(data)
summary(data)
sum(is.na(data))
str(data)

data$Sales=ifelse(data$Sales>=8,"high","low")
data$Sales=as.factor(data$Sales)
table(data$Sales)

library(rpart)
library(rpart.plot)
library(caret)
library(ROSE)
set.seed(1)
index<-createDataPartition(data$Sales, p= .75, list=FALSE)
Train<-data[index,]
table(Train$Sales)

train_over=ovun.sample(Sales~.,data = Train,method = "over",N=354)$data
Test<-data[-index,]
table(train_over$Sales)

model1<-rpart(Sales~., data=train_over)
pred<-predict(model1, Test, type="class")
confusionMatrix(pred, Test$Sales)



