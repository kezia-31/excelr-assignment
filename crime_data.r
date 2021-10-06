data=read.csv(file.choose())
View(data)
x=data[,-1]
newdata=scale(x)
summary(newdata)

d=dist(newdata,method = "euclidean")
fit=hclust(d,method = "complete")
plot(fit)
plot(fit,hang = -1)
groups=cutree(fit,k=5)
table(groups)	
rect.hclust(fit,k=5,border = "red")

d=dist(newdata,method = "euclidean")
fit=hclust(d,method = "single")
plot(fit)
plot(fit,hang = -1)
groups=cutree(fit,k=5)
table(groups)
rect.hclust(fit,k=5,border = "red")

d=dist(newdata,method = "euclidean")
fit=hclust(d,method = "average")
plot(fit)
plot(fit,hang = -1)
groups=cutree(fit,k=4)
table(groups)
rect.hclust(fit,k=4,border = "red")

d=dist(newdata,method = "euclidean")
fit=hclust(d,method = "complete")
plot(fit)
plot(fit,hang = -1)
groups=cutree(fit,k=4)
table(groups)
rect.hclust(fit,k=4,border = "red")

membership=as.matrix(groups)
final=data.frame(data,membership)
view(final)

data=read.csv(file.choose())
attach(data)
View(data)
x=data[,-(1)]
data1=scale()
wss = kmeans(data1,centers = i) 
for (i in 2:15) wss[i] = sum(kmeans(data2, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")  
title(sub = "K-Means Clustering Scree-Plot")


