data=read.csv(file.choose())
attach(data)
data=data[,-4]
summary(data)
cor(data)
pairs(data)

model1=lm(Profit~R.D.Spend+Administration+Marketing.Spend)
library(car)
influencePlot(model1)

influenceIndexPlot(model1)

final.model=lm(Profit~R.D.Spend+Marketing.Spend,data = data[-c(50,46,15),])
summary(final.model)

pred=predict(final.model,data=data[-c(15,46,50),])
rmse(data[-c(15,46,50),]$Profit,pred)
residual=residuals(final.model)

shapiro.test(residual)


                