library(arules)
data=read.csv(file.choose())
View(data)
book=as(as.matrix(data),"transactions")class(data)
class(book)
summary(book)
arules=apriori(book,parameter = list(support=0.002,confidence=0.6,minlen=3))
arules
rules=inspect(head(sort(arules,by="lift")))
head(quality(arules))
arules=apriori(book,parameter = list(support=0.001,confidence=0.1,minlen=3))
arules
rules=inspect(head(sort(arules,by="lift")))
head(quality(arules))
