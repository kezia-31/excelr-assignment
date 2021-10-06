install.packages("readxl")
library("readxl")
data <- read_excel(file.choose())
attach(data)
View(data)
x=data[,-c(1,4,5,6,12)]
data1=scale(x)
wss = kmeans(data1,centers = i)$wss	 
for (i in 2:20) wss[i] = sum(kmeans(data1, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   
title(sub = "K-Means Clustering Scree-Plot")

