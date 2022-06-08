#Carico pacchetti necessari
library(readxl)
library(tidyverse)

########################################################

#Importo il dataset
dati<-readxl::read_xlsx("data.xlsx", col_names = T)
#Tolgo colonna che contava le osservazioni
dati <- dati[2:ncol(dati)]
#Trasformo tutti i numeri in variabili stringa
dati <- lapply(dati, as.character)
dati <- as.data.frame(dati)

########################################################

#Creo un lista vuota
lista <- vector("list", length = ncol(dati))
#Nomino gli elementi della lista come le colonne del DF
for (i in 1:ncol(dati)){
  names(lista)[i]<- colnames(dati[i])
}

########################################################

#Genero i vettori fattoriali partendo dal file legenda
Sede <- as.factor(c("1" = "Urbana", "2" = "Rurale"))
Sede
Eta <- factor(c("1" = "25-30", "2" = "30-39", "3" = "40-49",
                   "4" = "50-59", "5"= ">60"), 
              levels = c("25-30", "30-39", "40-49", "50-59", ">60"))

N_assistiti<- factor(c("1" = "<500", "2" = "500-1000", 
                          "3" = "1000-1500", "4" = ">1500"),
                     levels = c("<500", "500-1000", "1000-1500",
                                ">1500"))

Genere <- factor(c("1" = "M", "2" = "F"))

Specializzazione <- factor(c("1" = "Si", "2" = "No"))

CorsoMMG <- factor(c("1" = "Si", "2" = "No"))

Televisite_pz <- factor(c("1" = "Mai", "2" = "Raramente", "3" = "Se richiesta",
                             "4" = "Abitualmente"),
                           levels = c("Mai", "Raramente", "Se richiesta", "Abitualmente"))

u30gg_televisite <- factor(c("1" = "0", "2" = "<10", "3" = "10-100",
                                "4" = ">100"),
                           levels = c("0", "<10", "10-100", ">100"))

Frequenza_televisite <- factor(c("1" = "Mai non disposto", "2" = "Mai disposto",
                                    "3" = "Ogni volta"))

Specialisti_televisite <- factor(c("1" = "Cardiologo", "2" = "Dermatologo",
                                      "3" = "Neurologo","4" = "Pneumologo",
                                      "5" = "Radiologo", "6" = "Pediatra",
                                      "7" = "altro"))

Specialisti_televisite2 <- factor(c("1" = "Cardiologo", "2" = "Dermatologo",
                                       "3" = "Neurologo","4" = "Pneumologo",
                                       "5" = "Radiologo", "6" = "Pediatra",
                                       "7" = "altro"))

Prof_teleconsulto <- factor(c("1" = "MMG-Specialista", "2" = "Infermiere distretto-MMG"))

Pz_televisita <- factor(c("1" = "Acuto", "2" = "Cronico",
                             "3" = "Grande anziano",
                             "4" = "Cronico riacutizzato"))

Applicazione_televisita <- factor(c("1" = "Telerefertazione",
                                       "2" = "Telemonitoraggio dopo primo accesso",
                                       "3" = "Teleconsulto",
                                       "4" = "Videochiamata"))

Conduzione_studio <- factor(c("1" = "Medicina in rete", "2" = "Medicina in gruppo",
                                 "3" = "Medicina di associazione",
                                 "4" = "Studio singolo"))

Ingresso_CDC <- factor(c("1" = "Si", "2" = "No", "3" = "Dipende da regole"))

CDC_telemedicina <- factor(c("1" = "Si, se mi verrà fornita la tecnologia",
                                "2" = "No, la applico già"))

########################################################

#Carico i vettori fattoriali nella lista
lista[[1]]<- Sede
lista[[2]]<- Eta
lista[[3]]<- N_assistiti
lista[[4]]<- Genere
lista[[5]]<- Specializzazione
lista[[6]]<- CorsoMMG
lista[[7]]<- Televisite_pz
lista[[8]]<- u30gg_televisite
lista[[9]]<- Frequenza_televisite
lista[[10]]<- Specialisti_televisite
lista[[11]]<- Specialisti_televisite2
lista[[12]]<- Prof_teleconsulto
lista[[13]]<- Pz_televisita
lista[[14]]<- Applicazione_televisita
lista[[15]]<- Conduzione_studio
lista[[16]]<- Ingresso_CDC
lista[[17]]<- CDC_telemedicina

########################################################

#Lancio ciclo for per trasformare i numeri del DF in
#valori fattoriali, talvolta anche ordinati
for (i in 1:ncol(dati)){
  
  #genero un vettore fatto dalle variabili fattoriali
  v <- unlist(dati[names(lista)[i]])
  #elimino i nomi (non è fondamentale)
  names(v)<- NULL
  #sovrascrivo con questi dati il DF di partenza
  dati[i]<-lista[[i]][v]
  #elimino il vettore per liberare spazio
  rm(v)
}

########################################################

#osservo i dati
View(dati)

########################################################

#genero grafico
ggplot(dati, aes(x = N_assistiti,fill=Televisite_pz))+
  geom_bar(stat="count", color="black", width = 0.5)+
  facet_wrap(~dati$Eta)+
  theme_bw()+
  ggtitle("Televisita MMG Asti")+
  xlab("Numero di assistiti")+
  ylim(c(0, 30))+
  ylab("Percentuale")+
  theme(legend.position = "bottom")


ggplot(dati, aes(x = CDC_telemedicina))+
  geom_bar(stat = "count", width = 0.3)

