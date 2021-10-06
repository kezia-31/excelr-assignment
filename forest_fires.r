install.packages("neuralnet")
library(neuralnet)
forestfire=read.csv(file.choose())
attach(forestfire)
View(forestfire)
summary(forestfire)

forestfire=forestfire[,-c(1,2)]
forestfire$size_category=ifelse(forestfire$size_category=="small",0,1)
sum(is.na(forest))

str(forestfire)

normalize<-function(x){+ return ( (x-min(x))/(max(x)-min(x)))+ }
forestfire_norm<-as.data.frame(lapply(forestfire[,c(1:9)],FUN=normalize))
forestfire_norm=cbind(forestfire_norm,forest[,c(10:29)])


forestfire_train=forest_norm[1:388,]
forestfire_test=forest_norm[389:517,]

library(nnet)
set.seed(300)

model1 <- neuralnet(area~.,data = forest_train)
plot(model1)

model_results <- compute(model1,forest_test[-9])
predicted_area <- model_results$net.result
cor(predicted_area,forest_test$area)

plot(predicted_area,forest_test$area)

