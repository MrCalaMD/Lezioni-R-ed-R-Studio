
rm(list=ls())
setwd("/Users/eciliberto/Documents/corsi_statistica/R/R_definitivo/Introduction_R/data")  ###define your working directory (the folder where you saved data)
data<-read.table("burn1000.csv",sep=",",head=T)
dim(data)
names(data)
head(data)
# We can get basic descriptives for the entire data set by using summary
summary(data)
#Declare the categorical variables as factor variables.
data$gender<-as.factor(data$gender)
data$flame<-as.factor(data$flame)
data$inh_inj<-as.factor(data$inh_inj)
data$race<-as.factor(data$race)
#distribution of continuous variables
summary(data$age[data$death==1])
summary(data$age[data$death==2])
t.test(data$age[data$death==1],data$age[data$death==2])
summary(data$tbsa[data$death==1])
summary(data$tbsa[data$death==2])
t.test(data$tbsa[data$death==1],data$tbsa[data$death==2])
#cross-tabulation
table(data$death,data$gender)
prop.table(table(data$death,data$gender))
chisq.test(table(data$death,data$gender))
table(data$death,data$flame)
prop.table(table(data$death,data$flame))
chisq.test(table(data$death,data$flame))
table(data$death,data$inh_inj)
prop.table(table(data$death,data$inh_inj))
chisq.test(table(data$death,data$inh_inj))
table(data$death,data$race)
prop.table(table(data$death,data$race))
chisq.test(table(data$death,data$race))
#logistic model with age
data$death[data$death==1]<-0
data$death[data$death==2]<-1
mylogit.age<-glm(death~age,family = binomial, data = data)
summary(mylogit.age)
confint(mylogit.age)
exp(coef(mylogit.age))
exp(cbind(OR = coef(mylogit.age), confint(mylogit.age)))
#logistic model with tbsa
mylogit.tbsa<-glm(death~tbsa,family = binomial, data = data)
summary(mylogit.tbsa)
confint(mylogit.tbsa)
exp(coef(mylogit.tbsa))
exp(cbind(OR = coef(mylogit.tbsa), confint(mylogit.tbsa)))
#logistic model with inh
mylogit.inj<-glm(death~inh_inj,family = binomial, data = data)
summary(mylogit.inj)
confint(mylogit.inj)
exp(coef(mylogit.inj))
exp(cbind(OR = coef(mylogit.inj), confint(mylogit.inj)))
#logistic model with gender
mylogit.gender<-glm(death~gender,family = binomial, data = data)
summary(mylogit.gender)
confint(mylogit.gender)
exp(coef(mylogit.gender))
exp(cbind(OR = coef(mylogit.gender), confint(mylogit.gender)))
#logistic model with flame
mylogit.flame<-glm(death~flame,family = binomial, data = data)
summary(mylogit.flame)
confint(mylogit.flame)
exp(coef(mylogit.flame))
exp(cbind(OR = coef(mylogit.flame), confint(mylogit.flame)))
#logistic model with race
mylogit.race<-glm(death~race,family = binomial, data = data)
summary(mylogit.race)
confint(mylogit.race)
exp(coef(mylogit.race))
exp(cbind(OR = coef(mylogit.race), confint(mylogit.race)))
#multivariable logistic model
mylogit<-glm(death~age+tbsa+inh_inj+factor(gender)+factor(flame)+factor(race),family = binomial,data = data)
summary(mylogit)
confint(mylogit)
exp(coef(mylogit))
exp(cbind(OR = coef(mylogit), confint(mylogit)))
#prediction
newdata<-with(data, data.frame(age=mean(age),tbsa=mean(tbsa),inh_inj=2,
                               gender=2, flame=2,race=1:2))
newdata$inh_inj<-as.factor(newdata$inh_inj)
newdata$gender<-as.factor(newdata$gender)
newdata$flame<-as.factor(newdata$flame)
prediction<-predict(mylogit,newdata,type="response")
prediction
