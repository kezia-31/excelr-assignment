data=read.csv(file.choose())
attach(data)
`View(data)
data<-data[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
sum(is.na(data))
cor(data)
model_price=lm(Price~.,data = data)
summary(model_price)

influencePlot(model_price)

final_mmodel=lm(Price~.,data=data[-c(222,602,961,81),])
summary(final_model)

pred=predict(final_model)
library(Metrics)
rmse(data[-c(222,602,961,81),]$Price,pred)
residual=model5$residuals
qqnorm(residual)
qqline(residual)

