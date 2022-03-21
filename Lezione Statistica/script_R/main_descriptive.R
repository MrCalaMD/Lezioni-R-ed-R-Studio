library(foreign)
#setwd("/Users/eciliberto/Documents/corsi_statistica/R/R_definitivo/Introduction_R/data")  ###define your working directory (the folder where you saved data)
data<-read.csv("data.births.csv",header = TRUE, sep = ",", row.names = 1)
View(data)
data[data$id==7,]
data$bweight[1:20]
str(data)
summary(data)
summary(data$bweight)
summary(data$gestwks)
summary(data$matage)
par(mfrow=c(3,2))
hist(data$bweight,main="Histogram",xlab="Birth weight") 
plot(density(data$bweight),main="Density function" ,xlab="Birth weight")
hist(data$gestwks,main="Histogram",xlab="Gestational weeks") 
plot(density(data$gestwks[which(data$gestwks!="NA")]),main="Density function" ,
xlab="Gestational weeks")
hist(data$matage,main="Histogram",xlab="Maternal age") 
plot(density(data$matage),main="Density function" ,xlab="Maternal age")
par(0, 0, 0, 0, mfrow=c(1,3))
boxplot(data$bweight,xlab="Birth weight") 
boxplot(data$gestwks,xlab="Gestational weeks") 
boxplot(data$matage,xlab="Maternal age") 
data$preterm<-factor(data$preterm,levels=c(0,1),labels=c("in term","preterm"))
data$hyp<-factor(data$hyp,levels=c(0,1), labels=c("no hyp", "hyp"))
data$sex <- factor(data$sex,levels = c(1,2),labels = c("male", "female"))
summary(data)
table(data$preterm)
table(data$hyp)
table(data$sex)
par(1, 1, 1, 1, mfrow=c(1,3))
pie(table(data$preterm))
pie(table(data$hyp))
pie(table(data$sex))
gest.cat<-cut(data$gestwks,breaks=c(0,20,35,37,39,45),right=F)
table(gest.cat)
par(mfrow=c(1,1))
hist(data$gestwks,c(0,20,35,37,39,45),main="Histogram",xlab="Gestational weeks") 
data$early[data$gestwks<30]<-0
data$early[data$gestwks>=30]<-1
data$id[data$early==0]
length(data$gestwks[is.na(data$gestwks)])
################################################################################

par(mfrow= c(1, 1))

plot(data$gestwks, data$bweight, xlab = "Settimane di gestazione", ylab = "Peso alla nascita",
     main = "Rapporto Sett.Gestazione e peso alla nascita")


linear.model <- lm(data$gestwks ~ data$bweight)

summary(linear.model)
