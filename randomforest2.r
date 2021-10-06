data=read.csv(file.choose())
attach(data)
View(data)

summary(data)

sum(is.na(data))
str(data)


data$Sales=ifelse(data$Sales>=8,"high","low")
data$Sales=as.factor(data$Sales)
table(data$Sales)

library(randomForest)
library(ROSE)
library(caret)

set.seed(1)

index<-createDataPartition(data$Sales, p= .7, list=FALSE)
Train<-data[index,]
table(Train$Sales)

train_over=ovun.sample(Sales~.,data = Train,method = "over",N=354)$data
Test<-data[-index,]
table(Train$Sales)

set.seed(1)

fit.forest <- randomForest(Sales~.,data=Train, na.action=na.roughfix,importance=TRUE)

mean(Train$Sales==predict(fit.forest,Train))
plot(fit.forest,lwd=2)

pred_train <- predict(fit.forest,train_over)
library(caret)
confusionMatrix(Train$Sales, pred_train)

pred_test <- predict(fit.forest,newdata=Test)
mean(pred_test==Test$Sales)

confusionMatrix(Test$Sales, pred_test)

library(randomForest)
set.seed(1)


index<-createDataPartition(data$Sales, p= .7, list=FALSE)
Train<-data[index,]
table(Train$Sales)

Test<-data[-index,]
table(Train$Sales)

fit.forest <- randomForest(Sales~.,data=Train, na.action=na.roughfix,importance=TRUE)
mean(Train$Sales==predict(fit.forest,Train))

plot(fit.forest,lwd=2)

library(caret)
confusionMatrix(Train$Sales, pred_train)
pred_test <- predict(fit.forest,newdata=Test)

mean(pred_test==Test$Sales)

confusionMatrix(Test$Sales, pred_test)










