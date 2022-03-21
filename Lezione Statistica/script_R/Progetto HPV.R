rm(list = ls())
library(tidyverse)

maschi <- read.csv("maschi.txt", header = F, sep = " ")

colnames(maschi) <- c("ASL","Anno 2006", "Anno 2007", "Anno 2008")

maschi <- data.frame(maschi)

maschi <- pivot_longer(data = maschi,
             starts_with("Anno"),
             names_to = "Coorte", 
             values_to = "Percentuale")
maschi$Percentuale = gsub(",", ".",maschi$Percentuale)
maschi$Percentuale = as.numeric(maschi$Percentuale)

ggplot(maschi, aes(x = ASL, y = Percentuale, color = Coorte,
                   fill = Coorte,
                   group = Coorte))+
  geom_col(position = "dodge")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6))+
  ggtitle("Percentuale maschi vaccinati HPV")

###############################################################################
          
femmine <- read.csv("femmine.txt", header = F, sep = " ")

colnames(femmine) <- c("ASL","Anno 2001", "Anno 2002", "Anno 2003",
                      "Anno 2004", "Anno 2005", "Anno 2006", "Anno 2007",
                      "trend")

maschi <- data.frame(maschi)

femmine <- pivot_longer(data = femmine,
                       starts_with("Anno"),
                       names_to = "Coorte", 
                       values_to = "Percentuale")

femmine$Percentuale = gsub(",", ".",femmine$Percentuale)
femmine$Percentuale = as.numeric(femmine$Percentuale)

ggplot(femmine %>% 
         filter(Coorte %in% c("Anno 2005", "Anno 2006","Anno 2007")), aes(x = ASL, y = Percentuale, color = Coorte,
                   fill = Coorte,
                   group = Coorte))+
  geom_col(position = "dodge")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6))+
  ggtitle("Percentuale femmine vaccinate HPV")

femmine %>% 
  select(Coorte %in% c("Anno 2005", "Anno 2006","Anno 2007"))

