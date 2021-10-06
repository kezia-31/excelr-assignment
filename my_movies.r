library("arules")
data=read.csv(file.choose())
View(data)
class(data)
y=data[-c(1,2,3,4,5)]
y=as(as.matrix(x),"transactions")
class(y)
summary(y)
arules=apriori(y,parameter = list(support=0.002,confidence=0.6,minlen=3)
arules
rules=inspect(head(sort(arules,by="lift")))
head(quality(arules))
arules=apriori(data2,parameter = list(support=0.001,confidence=0.05,minlen=1))
arules
rules=inspect(head(sort(arules,by="lift")))
head(quality(arules))
               