gasturbines<-read.csv(file.choose())
attach(gasturbines)
View(gasturbines)
summary(gasturbines)

str(gasturbines)

gasturbines_norm<-as.data.frame(lapply(gasturbines[,1:11], FUN = normalise))