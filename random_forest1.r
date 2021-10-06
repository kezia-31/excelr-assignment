data=read.csv(file.choose())
attach(data)
View(data)
summary(data)

sum(is.na(data))
str(data)

data$Taxable.Income=ifelse(data$Taxable.Income<=30000,"risky","good")
data$Taxable.Income=as.factor(data$Taxable.Income)
table(data$Taxable.Income)
library(randomForest)
library(caret)
library(ROSE)
set.seed(1)

index<-createDataPartition(data$Taxable.Income, p= .7, list=FALSE)
Train<-data[index,]
train_over=ovun.sample(Taxable.Income~.,data = Train,method = "over",N=668)$data
Test<-data[-index,]
table(Train$Taxable.Income)
set.seed(1)

fit.forest <- randomForest(Taxable.Income~.,data=train_over, na.action=na.roughfix,importance=TRUE)
mean(train_over$Taxable.Income==predict(fit.forest,train_over))

plot(fit.forest,lwd=2)

pred_train <- predict(fit.forest,train_over)
confusionMatrix(train_over$Taxable.Income, pred_train)

pred_test <- predict(fit.forest,newdata=Test)
mean(pred_test==Test$Taxable.Income)

confusionMatrix(Test$Taxable.Income, pred_test)


