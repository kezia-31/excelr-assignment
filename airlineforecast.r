airline=read.csv(file.choose())> X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==")+0 )
colnames(X)=month.abb
air_data=cbind(airline,X)
library("tseries")
library("readr")
colnames(air_data)

air_data["t"]=c(1:96)
air_data["log_passegers"]=log(air_data$Passengers)
air_data["t_square"]=(air_data$t)*(air_data$t)
attach(air_data)

train=air_data[1:84,]
test=air_data[85:96,]

linear_model=lm(Passengers~t,data=train)
summary(linear_model)

linear_pred=predict(linear_model,interval='predict',newdata=test)
library("caret")
rmse_linear=RMSE(test$Passengers,linear_pred)
rmse_linear

expo_model=lm(log_passegers~t,data=train)
summary(expo_model)

expo_pred=predict(expo_model,interval='predict',newdata=test)
pred_actual=exp(expo_pred)
rmse_expo=RMSE(test$Passengers,pred_actual)
rmse_expo

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-predict(Quad_model,interval='predict',newdata=test)
rmse_quad=RMSE(test$Passengers,Quad_pred)
rmse_quad


add_sea_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(add_sea_model)

add_sea_pred<-predict(add_sea_model,newdata=test,interval='predict')
rmse_add_sea=RMSE(test$Passengers,add_sea_pred)
rmse_add_sea

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)

Add_sea_Quad_pred<-predict(Add_sea_Quad_model,interval='predict',newdata=test)
rmse_add_sea_quad=RMSE(test$Passengers,Add_sea_Quad_pred)
rmse_add_sea_quad

multi_sea_model<-lm(log_passegers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)


multi_sea_pred<-predict(multi_sea_model,newdata=test,interval='predict')
actual_multi_sea_pred=exp(multi_sea_pred)
rmse_multi_sea=RMSE(test$Passengers,actual_multi_sea_pred)
rmse_multi_sea

multi_add_sea_model<-lm(log_passegers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model)

multi_add_sea_pred<-predict(multi_add_sea_model,newdata=test,interval='predict')
actual_multi_add_sea_pred=exp(multi_add_sea_pred)
rmse_multi_add_sea=RMSE(test$Passengers,actual_multi_add_sea_pred)
rmse_multi_add_sea

rmse_table=data.frame(c("linear","expo","Quad","add_sea","Add_sea_Quad","multi_sea","multi_add_sea"),c(rmse_linear,rmse_expo,rmse_quad,rmse_add_sea,rmse_add_sea_quad,rmse_multi_sea,rmse_multi_add_sea))

colnames(rmse_table)

colnames(rmse_table)<-c("models","RMSE")
View(rmse_table)

rmse_table

plot(multi_add_sea_model)

acf(multi_add_sea_model$residuals,lag.max = 12)
A=arima(multi_add_sea_model$residuals,order = c(1,0,0))

arima_errors=A$residuals
acf(arima_errors,lag.max = 12)

library(forecast)

errors_12=forecast(A,h=12)
View(errors_12)

future_error=data.frame(errors_12)
class(future_error)

future_errors=future_error$Point.Forecast
future_errors

predicted_new_values=actual_multi_add_sea_pred+future_errors
predicted_new_values

