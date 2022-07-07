install.packages("ggplot2")                          
library("ggplot2")

library(magrittr)
install.packages("multipanelfigure")
library("multipanelfigure")

install.packages("tidyverse")
library(tidyverse)

install.packages("purrr")
library(purrr)

install.packages("dplyr")
library("dplyr")


podaciNogomet <- read.table("podaci.csv", header = TRUE, sep = ';')

calculateWinningProbability<- function(pob,ner,brUtak){
  prob<-c((pob+ner/2)/brUtak)
  
}
probability<-calculateWinningProbability(podaciNogomet$Pobjede, podaciNogomet$Nerijesene,podaciNogomet$BrojUtakmica)

podaciNogomet$VjerojatnostPobjede<- probability


ggplot(podaciNogomet, aes(x = Reprezentacija , y=Pobjede)) +        
  geom_bar(stat = "identity",fill = "green" )+
  coord_flip()

ggplot(podaciNogomet, aes(x = Reprezentacija , y=Porazi)) +        
  geom_bar(stat = "identity",fill = "red" )+
  coord_flip()


ggplot(podaciNogomet, aes(x = Reprezentacija , y=Nerijesene)) +        
  geom_bar(stat = "identity",fill = "yellow" )+
  coord_flip()

ggplot(podaciNogomet, aes(x = Reprezentacija , y=VjerojatnostPobjede)) +        
  geom_bar(stat = "identity",fill = "steelblue" )+
  coord_flip()



simulacijaUtakmice <- function(zavrsnica_Reprezentacije,zavrsnica_VjerojatnostPobjede){
  Pobjednik <- sample (zavrsnica_Reprezentacije, zavrsnica_VjerojatnostPobjede, replace = TRUE, size = 1)
  return  (Pobjednik)
}

dohvatiVjerojatnost <- function(Pobjednik, podaciNogomet){
  PobjednikVjerojatnost<-0
  PobjednikVjerojatnost=podaciNogomet%>%
    filter(Reprezentacija==Pobjednik)%>%
    select(VjerojatnostPobjede)
  return  (PobjednikVjerojatnost)
}
 
simulacijaZavrsnice <- function(podaciNogomet){
# OSMINA FINALA ================================================================================================

PobjedniciOsmineFinala<-c()
OsminaFinalaVjerojatnost<-c()

TrenutniPobjednik <-simulacijaUtakmice(podaciNogomet$Reprezentacija[c(1,2)],podaciNogomet$VjerojatnostPobjede[c(1,2)])
PobjedniciOsmineFinala<-append(PobjedniciOsmineFinala, TrenutniPobjednik )

TrenutnaVjerojatnost<-dohvatiVjerojatnost(TrenutniPobjednik, podaciNogomet)
OsminaFinalaVjerojatnost<-append(OsminaFinalaVjerojatnost,TrenutnaVjerojatnost)

######################################################################################################

TrenutniPobjednik <-simulacijaUtakmice(podaciNogomet$Reprezentacija[c(3,4)],podaciNogomet$VjerojatnostPobjede[c(3,4)])
PobjedniciOsmineFinala<-append(PobjedniciOsmineFinala, TrenutniPobjednik )

TrenutnaVjerojatnost<-dohvatiVjerojatnost(TrenutniPobjednik, podaciNogomet)
OsminaFinalaVjerojatnost<-append(OsminaFinalaVjerojatnost,TrenutnaVjerojatnost)

######################################################################################################

TrenutniPobjednik <-simulacijaUtakmice(podaciNogomet$Reprezentacija[c(5,6)],podaciNogomet$VjerojatnostPobjede[c(5,6)])
PobjedniciOsmineFinala<-append(PobjedniciOsmineFinala, TrenutniPobjednik )

TrenutnaVjerojatnost<-dohvatiVjerojatnost(TrenutniPobjednik, podaciNogomet)
OsminaFinalaVjerojatnost<-append(OsminaFinalaVjerojatnost,TrenutnaVjerojatnost)

######################################################################################################

TrenutniPobjednik <-simulacijaUtakmice(podaciNogomet$Reprezentacija[c(7,8)],podaciNogomet$VjerojatnostPobjede[c(7,8)])
PobjedniciOsmineFinala<-append(PobjedniciOsmineFinala, TrenutniPobjednik )

TrenutnaVjerojatnost<-dohvatiVjerojatnost(TrenutniPobjednik,podaciNogomet)
OsminaFinalaVjerojatnost<-append(OsminaFinalaVjerojatnost,TrenutnaVjerojatnost)

######################################################################################################

TrenutniPobjednik <-simulacijaUtakmice(podaciNogomet$Reprezentacija[c(9,10)],podaciNogomet$VjerojatnostPobjede[c(9,10)])
PobjedniciOsmineFinala<-append(PobjedniciOsmineFinala, TrenutniPobjednik )

TrenutnaVjerojatnost<-dohvatiVjerojatnost(TrenutniPobjednik,podaciNogomet)
OsminaFinalaVjerojatnost<-append(OsminaFinalaVjerojatnost,TrenutnaVjerojatnost)

######################################################################################################

TrenutniPobjednik <-simulacijaUtakmice(podaciNogomet$Reprezentacija[c(11,12)],podaciNogomet$VjerojatnostPobjede[c(11,12)])
PobjedniciOsmineFinala<-append(PobjedniciOsmineFinala, TrenutniPobjednik )

TrenutnaVjerojatnost<-dohvatiVjerojatnost(TrenutniPobjednik,podaciNogomet)
OsminaFinalaVjerojatnost<-append(OsminaFinalaVjerojatnost,TrenutnaVjerojatnost)

######################################################################################################

TrenutniPobjednik <-simulacijaUtakmice(podaciNogomet$Reprezentacija[c(13,14)],podaciNogomet$VjerojatnostPobjede[c(13,14)])
PobjedniciOsmineFinala<-append(PobjedniciOsmineFinala, TrenutniPobjednik )

TrenutnaVjerojatnost<-dohvatiVjerojatnost(TrenutniPobjednik,podaciNogomet)
OsminaFinalaVjerojatnost<-append(OsminaFinalaVjerojatnost,TrenutnaVjerojatnost)

######################################################################################################

TrenutniPobjednik <-simulacijaUtakmice(podaciNogomet$Reprezentacija[c(15,16)],podaciNogomet$VjerojatnostPobjede[c(15,16)])
PobjedniciOsmineFinala<-append(PobjedniciOsmineFinala, TrenutniPobjednik )

TrenutnaVjerojatnost<-dohvatiVjerojatnost(TrenutniPobjednik,podaciNogomet)
OsminaFinalaVjerojatnost<-append(OsminaFinalaVjerojatnost,TrenutnaVjerojatnost)

#CETVRTINA FINALA ================================================================================================

PobjedniciCetvrtineFinala<-c()
CetvrtinaFinalaVjerojatnost<-c()

TrenutniPobjednik <-simulacijaUtakmice(PobjedniciOsmineFinala[1:2],OsminaFinalaVjerojatnost[1:2])
PobjedniciCetvrtineFinala<-append(PobjedniciCetvrtineFinala, TrenutniPobjednik )

TrenutnaVjerojatnost<-dohvatiVjerojatnost(TrenutniPobjednik, podaciNogomet)
CetvrtinaFinalaVjerojatnost<-append(CetvrtinaFinalaVjerojatnost,TrenutnaVjerojatnost)

######################################################################################################

TrenutniPobjednik <-simulacijaUtakmice(PobjedniciOsmineFinala[3:4],OsminaFinalaVjerojatnost[3:4])
PobjedniciCetvrtineFinala<-append(PobjedniciCetvrtineFinala, TrenutniPobjednik )

TrenutnaVjerojatnost<-dohvatiVjerojatnost(TrenutniPobjednik, podaciNogomet)
CetvrtinaFinalaVjerojatnost<-append(CetvrtinaFinalaVjerojatnost,TrenutnaVjerojatnost)

######################################################################################################

TrenutniPobjednik <-simulacijaUtakmice(PobjedniciOsmineFinala[5:6],OsminaFinalaVjerojatnost[5:6])
PobjedniciCetvrtineFinala<-append(PobjedniciCetvrtineFinala, TrenutniPobjednik )

TrenutnaVjerojatnost<-dohvatiVjerojatnost(TrenutniPobjednik,podaciNogomet)
CetvrtinaFinalaVjerojatnost<-append(CetvrtinaFinalaVjerojatnost,TrenutnaVjerojatnost)

######################################################################################################

TrenutniPobjednik <-simulacijaUtakmice(PobjedniciOsmineFinala[7:8],OsminaFinalaVjerojatnost[7:8])
PobjedniciCetvrtineFinala<-append(PobjedniciCetvrtineFinala, TrenutniPobjednik )

TrenutnaVjerojatnost<-dohvatiVjerojatnost(TrenutniPobjednik,podaciNogomet)
CetvrtinaFinalaVjerojatnost<-append(CetvrtinaFinalaVjerojatnost,TrenutnaVjerojatnost)

#POLUFINALE ================================================================================================

PobjedniciPolufinala<-c()
PolufinaleVjerojatnost<-c()

TrenutniPobjednik <-simulacijaUtakmice(PobjedniciCetvrtineFinala[1:2],CetvrtinaFinalaVjerojatnost[1:2])
PobjedniciPolufinala<-append(PobjedniciPolufinala, TrenutniPobjednik )

TrenutnaVjerojatnost<-dohvatiVjerojatnost(TrenutniPobjednik,podaciNogomet)
PolufinaleVjerojatnost<-append(PolufinaleVjerojatnost,TrenutnaVjerojatnost)


######################################################################################################

TrenutniPobjednik <-simulacijaUtakmice(PobjedniciCetvrtineFinala[3:4],CetvrtinaFinalaVjerojatnost[3:4])
PobjedniciPolufinala<-append(PobjedniciPolufinala, TrenutniPobjednik )

TrenutnaVjerojatnost<-dohvatiVjerojatnost(TrenutniPobjednik,podaciNogomet)
PolufinaleVjerojatnost<-append(PolufinaleVjerojatnost,TrenutnaVjerojatnost)

#FINALE ================================================================================================

PobjednikFinala<-c()
FinaleVjerojatnost<-c()

TrenutniPobjednik <-simulacijaUtakmice(PobjedniciPolufinala[1:2],PolufinaleVjerojatnost[1:2])
PobjednikFinala<-append(PobjednikFinala, TrenutniPobjednik )

TrenutnaVjerojatnost<-dohvatiVjerojatnost(TrenutniPobjednik,podaciNogomet)
FinaleVjerojatnost<-append(FinaleVjerojatnost,TrenutnaVjerojatnost)

PobjednikFinala
FinaleVjerojatnost
return(PobjednikFinala)
}

rezultati <- rerun(1000, simulacijaZavrsnice(podaciNogomet))

Osvajanja<-as.data.frame(table(unlist(rezultati)))
colnames(Osvajanja)<-c('Reprezentacija', 'Broj_osvajanja_prvenstva')
view(Osvajanja)

ggplot(Osvajanja, aes(x = Reprezentacija , y=Broj_osvajanja_prvenstva)) +        
  geom_bar(stat = "identity",fill = "purple" )+
  coord_flip()

