data=read.csv(file.choose())
attach(data)
View(data)
summary(data)

sum(is.na(data))

str(data)

my_data=data[,-1]
scaled_data=scale(my_data)
pcaobj=princomp(scaled_data,cor = True,scores = True,covmat = NULL)
summary(pcaobj)

loadings(pca)
plot(pca)

pca$scores[,1:3]
my_data=cbind(data,pca$scores[,1:3])

clus_data=my_data[,15:17]
View(clus_data)
dist1=dist(clus_data,method = "euclidean")
fit1=hclust(dist1,method = "complete")
plot(fit1,hang = -1)
groups=cutree(fit1,5)
table(groups)

rect.hclust(fit1,k=5,border="red")
membership1=as.matrix(groups)
final1=data.frame(my_data,membership1)

clus_data=my_data[15:17]
View(clus_data)
wss = kmeans(clus_data,centers = i)$wss	 
for (i in 2:20) wss[i] = sum(kmeans(clus_data, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   
title(sub = "K-Means Clustering Scree-Plot")

fit2=kmeans(clus_data,13)
fit2$size
fit2$cluster

memership2=as.matrix(fit2$cluster)
final2=data.frame(my_data,memership2)

my_data=data[,-1]
scaled_data=scale(my_data)
dist=dist(scaled_data,method = "euclidean")
fit3=hclust(dist,method="complete")
plot(fit3,hang = -1)
rect.hclust(fit3,k=5,border="red")
groups2=cutree(fit3,k=5)
table(groups2)

memership3=as.matrix(groups2)

my_data=data[,-1]
scaled_data=scale(my_data)
wss=kmeans(scaled_data,centers = i)$wss
for (i in 2:20) wss[i] = sum(kmeans(scaled_data, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")  
title(sub = "K-Means Clustering Scree-Plot")

fit4=kmeans(scaled_data,13)
fit4$size

fit4$cluster

memership4=as.matrix(fit4$cluster)

output=data.frame(my_data,membership1,memership2,membership3,membership4)
View(output)

