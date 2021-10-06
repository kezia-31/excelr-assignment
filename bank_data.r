bank=read.csv(file.choose(),sep=";")
attach(bank)
View(bank)
summary(bank)
str(bank)

model <- glm(y~.,data=bank,family = "binomial")
summary(model)

prob <- predict(model,bank,type="response")
confusion<-table(prob>0.5,bank$y)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy

pred_values <- NULL
yes_no <- NULL
pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")
bank[,"prob"] <- prob
bank[,"pred_values"] <- pred_values
bank[,"yes_no"] <- yes_no
View(bank[,c(1,18:20)])
table(bank$y,bank$pred_values)

library(ROCR)
rocrpred<-prediction(prob,bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
str(rocrperf)



