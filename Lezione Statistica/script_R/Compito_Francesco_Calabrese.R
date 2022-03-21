rm(list=ls())
#setwd("/Users/eciliberto/Documents/corsi_statistica/R/R_definitivo/Introduction_R/data")  ###define your working directory (the folder where you saved data)
data<-read.csv("fev.csv",head=T,sep="")#ho messo il csv nella mia mia directory
dim(data)
# We can get basic descriptives for the entire data set by using summary
names(data)
head(data)
summary(data)
str(data)

#Esercizio 3: Plot an histogram of fever with an overlapping normal density curve.
hist(data$fev,c(0,1,2,3,4,5,6),freq=F,main="Histogram",xlab="FEV",ylim=c(0,0.6))
m<-mean(data$fev)
std<-sqrt(var(data$fev))
x<-seq(min(data$fev), max(data$fev), length = 40)
y<-dnorm(x, mean=m, sd=std)
lines(x,y,col="blue", lwd=2)

#Esercizio 4 : Graph a scatter plot of fever vs age and and include a linear fit.
plot(data$age,data$fev,xlab="age",ylab="FEV")
abline(lm(data$fev~data$age))

#Esercizio 5: Fit a linear regression model for fever vs age and examine the regression coefficients.
mylinear.age<-lm(fev~age,data=data)
summary(mylinear.age)
confint(mylinear.age, level = 0.95)

#Nel modello di regressione lineare l'intercetta è il valore che assume la variabile dipendente quando la 
#variabile indipendente è uguale a 0, graficamente il valore dell'intercetta è il punto dove la retta del 
#modello lineare incontra l'asse Y. In questo caso essa è pari a 0.43

#All'aumentare di un anno di vita la FEV aumenta di 0.222

#Sulla base dell'intervallo di confidenza posso concludere (con una confidenza del 95%) che la Fev dipende dall'età in quanto i limiti
#dell'intervallo di confidenza non includono il valore 0 e sono tutti e 2 maggiori di 0

#Esercizio 6: After generating a variable indicating the age centered at its mean value (cage)
#re-fit a linear regression model for fever vs cage and examine the regression coefficients.
data$cage <- data$age - mean(data$age)
mylinear.cage <- lm(fev~cage,data=data)
summary(mylinear.age)
confint(mylinear.age)
plot(data$cage,data$fev,xlab="age",ylab="FEV")
abline(lm(data$fev~data$cage))

#Nel modello di regressione lineare l'intercetta è il valore che assume la variabile dipendente quando la 
#variabile indipendente è uguale a 0, graficamente il valore dell'intercetta è il punto dove la retta del 
#modello lineare incontra l'asse Y. In questo caso essa è pari a 0.43

#Si possono trarre le stesse conclusioni sia usando l'età che l'età centrata sulla la media

#Sulla base dell'intervallo di confidenza posso concludere (con una confidenza del 95%) che la Fev dipende dall'età centrata 
#sulla media in quanto i limiti dell'intervallo di confidenza non includono il valore 0 e sono tutti e 2 maggiori di 0

#Esercizio 7: Graph a scatter plot of fever vs height and and include a linear fit.
plot(data$height,data$fev,xlab="Height",ylab="Fev")
abline(lm(data$fev~data$height), col= "red", lty = 2)

#Esercizio 8: After generating a variable indicating the height centered at its mean value (cheight)
#fit a linear regression model for fever vs cheight and examine the regression coefficients.
data$cheight<-data$height-mean(data$height)
mylinear.height<-lm(fev~cheight,data=data)
summary(mylinear.height)
confint(mylinear.height)

#Nel modello di regressione lineare l'intercetta è il valore che assume la variabile dipendente quando la 
#variabile indipendente è uguale a 0, graficamente il valore dell'intercetta è il punto dove la retta del 
#modello lineare incontra l'asse Y. In questo caso essa è pari a 0.13

#All'aumento di un unità dell'altezza centrata sulla media la FEV aumenta di 0.13

#Sulla base dell'intervallo di confidenza posso concludere (con una confidenza del 95%) che la Fev dipende dall'altezza centrata 
#sulla media in quanto i limiti dell'intervallo di confidenza 
#non includono il valore 0 e sono tutti e 2 maggiori di 0

#Esercizio 9: Fit a linear regression model for fever vs sex and examine the regression coefficients.
data$sex <- factor(data$sex)
plot(data$sex, data$fev, xlab="Sex", ylab="Fev")
mylinear.sex<-lm(fev~sex,data=data)
summary(mylinear.sex)
confint(mylinear.sex)

mean(data[data$sex == "female",]$fev)

#Nel modello di regressione lineare l'intercetta è il valore che assume la variabile dipendente quando la 
#variabile indipendente è uguale a 0. In questo caso essa è pari a 2.45, essendo la variabile dicotomica,
#l'intercetta dovrebbe rappresentare la FEV media delle femmine.

#L'essere maschi aumenta la FEV aumenta di 0.36

#Sulla base dell'intervallo di confidenza posso concludere (con una confidenza del 95%) che la Fev dipende dal sesso 
#in quanto i limiti dell'intervallo di confidenza non includono il valore 0 e sono tutti e 2 maggiori di 0

#Esercizio 10: Fit a linear regression model for fever vs smoke and examine the regression coefficients.
data$smoke <- factor(data$smoke)
mylinear.smoke<-lm(fev~smoke,data=data)
summary(mylinear.smoke)
confint(mylinear.smoke)
plot(data$smoke, data$fev, xlab="Smoke", ylab="Fev")

#Nel modello di regressione lineare l'intercetta è il valore che assume la variabile dipendente quando la 
#variabile indipendente è uguale a 0. In questo caso essa è pari a 3.27, essendo la variabile dicotomica,
#l'intercetta dovrebbe rappresentare la Fev dei non current smokers

#Il non essere current smokers riduce la Fev di 0.71 

#Sulla base dell'intervallo di confidenza posso concludere (con una confidenza del 95%) che la Fev dipende dall'essere  
#current smokers in quanto i limiti dell'intervallo di confidenza non includono il valore 0 e sono tutti e 2 minori di 0


data$age.cat<-cut(data$age,breaks=c(0,10,15,20),right=F)
table(data$age.cat)


mylinear.agecat<-lm(fev~age.cat,data=data)
summary(mylinear.agecat)
confint(mylinear.agecat)

#Esercizio 11: Fit a linear regression model for fever with all the variables and examine the regression coefficients.
mylinear<-lm(fev~cage+cheight+sex+smoke,data=data)
summary(mylinear)
confint(mylinear)

#Dal modello ottenuto si può desumere che l'età, l'altezza ed il sesso abbiano un influenza sulla Fev
#mentre il fumo, che nella regressione lineare semplice pareva avere una influenza sulla Fev
#non sembrerebbe alterarla in maniera statisticamente significativa.
#Probabilmente gli unici fumatori sono gli adolescenti che è normale abbiamo una Fev maggiore!


par(mfrow = c(2,1))
hist(data[data$smoke=="current smoker",]$age, xlim = c(0, 20),
     xlab = "Età fumatori", main = "Confronto fumatori non fumatori", breaks = c(seq(0,20,2)),
     col = "red")
hist(data[data$smoke=="non-current smoker",]$age, xlim = c(0, 20),
     xlab = "Età non fumatori", main = "", breaks = c(seq(0,20,2)),
     col = "green")

#Esercizio 12: Perform the residual’s analysis, by checking they are normally distributed.
qqnorm(residuals(mylinear))
shapiro.test(residuals(mylinear))

#Dal Q-Q Plot mi aspetto che i residui abbiano una distribuzione analoga a quella del campione teorico
#e che quindi i punti sul grafico si dispongano a formare una retta
#La parte centrale dei valori sembra avere una distribuzione normale, mentre gli estremi no

#Il test di Shapiro serve parte dall'ipotesi che la distribuzione sia normale (ipotesi nulla)
#Il risultato del test di Shapiro contraddice l'ipotesi nulla per cui i residui non hanno distribuzione normale

hist(residuals(mylinear))
plot(fitted(mylinear),residuals(mylinear),xlab="valori stimati",ylab="Residui")
abline(h=0)

newdata1<- with(data, data.frame(cage=0,cheight=0,sex=c("female","male","female","male"),smoke=c("current smoker","current smoker","non-current smoker","non-current smoker")))
newdata1
newdata2 <- cbind(newdata1, predict(mylinear, newdata = newdata1,se.fit = TRUE))
newdata2
pred.w.clim <- predict(mylinear, newdata1, interval = "confidence")
cbind(newdata2,pred.w.clim)

