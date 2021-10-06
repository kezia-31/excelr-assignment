data=read.csv(file.choose())
attach(data)
View(data)
summary(data)
sum(is.na(data))
str(data)

data$Taxable.Income=ifelse(data$Taxable.Income<=30000,"risky","good")
data$Taxable.Income=as.factor(data$Taxable.Income)
table(data$Taxable.Income)

library(rpart)
library(rpart.plot)
library(caret)
library(ROSE)

set.seed(1)
index<-createDataPartition(data$Taxable.Income, p= .8, list=FALSE)
Train<-data[index,]
train_over=ovun.sample(Taxable.Income~.,data = Train,method = "over",N=762)$data
Test<-data[-index,]
table(Train$Taxable.Income)

model1<-rpart(Taxable.Income~., data=train_over)
pred<-predict(model1, Test, type="class")
confusionMatrix(pred, Test$Taxable.Income)


