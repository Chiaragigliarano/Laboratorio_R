##################################################################
# Laboratorio di Statistica con R                                #                         
# Parte 1: analisi statistica e rappresentazioni grafiche con R. #
# creato da Chiara Gigliarano ( LIUC - Università Cattaneo)      #
##################################################################
# dal menu: Session > Set Working Directory > Choose Directory

setwd("/Users/chiaragigliarano/Desktop/ORIENTAMENTO_scuole/0_Laboratorio_R_CaioPlinio_Como_09.05.2024")

#===================================================#
###    STATISTICA DESCRITTIVA UNIVARIATA          ###
#===================================================#

# importiamo il dataset:
library(readxl)
dataset_2022_genere <- read_excel("dataset_2022_genere.xlsx")
View(dataset_2022_genere)
attach(dataset_2022_genere)

#---------------------------------------
# Misure ed indici statistici
#---------------------------------------

summary(dataset_2022_genere)

# oppure una variabile per volta:
summary(Laureati_M)

#---------------------------------------
# Rappresentazioni Grafiche
#---------------------------------------

# 1) Barplot:
barplot(height=Laureati_M, names=Regione, col="lightblue", las=2, main="% Laureati Maschi, 25-34 anni")

barplot(matrix(c(Laureati_M, Laureati_F), nrow=2, byrow=T), names=Regione, beside=T, col=c('lightblue', 'pink'), las=2, main="% Laureati 25-34 anni, per genere")
legend("topleft",c("M", "F"),col=c('lightblue', 'pink'),lty=c(1,2), cex=0.5,  box.lty=0)

# 2) ISTOGRAMMA BASATO SU FREQUENZE ASSOLUTE
par(mfrow=c(1,2))
hist(Neet_M, xlim=c(0,40), breaks=6, col="red", main="Neet_M", xlab="%")      
hist(Neet_F, xlim=c(0,40), breaks=6, col="green", main="Neet_F", xlab="%")

# 3) Boxplot multipli

par(mfrow=c(2,1))
boxplot(CompeAlfa_M,CompeAlfa_F, main="Competenze alfabetiche non adeguate", names=c("M","F"), 
        col=c( "lightblue", "pink"))
boxplot(CompeNume_M,CompeNume_F, main="Competenze numeriche non adeguate", names=c("M","F"), 
        col=c( "lightblue", "pink"))

# 4) Radar chart

library(fmsb)
# confrontiamo Lombardia e Calabria:
data2<- dataset_2022_genere[c(4, 18),]

# Aggiungiamo due righe nel dataframe:  max e min di ogni indicatore, da aggiungere nel grafico:
data2 <- rbind(rep(100,15) , rep(0,15) , data2)

radarchart( data2[,-1] , pcol=c("blue","red"))
legend("bottom",
       legend=c("Lombardia", "Calabria"),
       col=c("blue","red"),
       lty=c(1,2), box.lty=0, cex=0.5)

#===================================================#
###    STATISTICA DESCRITTIVA BIVARIATA           ###
#===================================================#

# importiamo il dataset:
dataset_2022_tot <- read_excel("dataset_2022_tot.xlsx")
View(dataset_2022_tot)
attach(dataset_2022_tot)

#Scatterplot

plot(Occupazione, SoddVita)
plot(SoddAmi, SoddVita, col="green")

pairs(cbind(SoddAmi, SoddFami, SoddVita))

# Correlazione 
cor(dataset_2022_tot[,-1])

# Regressione
modello<- lm(SoddVita~SoddAmi)
summary(modello)

#Aggiungo retta di regressione al grafico
plot(SoddAmi, SoddVita, col="purple")
abline(lm(SoddVita~SoddAmi), col="blue")


#===================================================#
###    MAPPE PER L'ITALIA                         ###
#===================================================#

# ----------------------------------------
# Occorre prima SCARICARE SHAPEFILES
# dal sito ISTAT: https://www.istat.it/it/archivio/222527
# scaricare: Confini amministrativi 2024 (zip)
# prendere quello "generalizzato"
# ----------------------------------------

# Gli shapefiles possono essere importati con la funzione st_read() 
library(sf)
library(ggplot2)

# dati regionali
reg2024 <- st_read("DATA/Limiti01012024_g/Reg01012024_g")
ggplot(data = reg2024) +
  geom_sf()

# --------------------------------
# MAPPE TEMATICHE
#Per rappresentare nelle mappe i valori di altre variabili, 
# basterà aggiungere i dati al dataframe dei dati spaziali.

colnames(dataset_2022_tot)[c(1)] <- c("DEN_REG")

# Ora procediamo alla costruzione della mappa:
library(dplyr)

right_join(reg2024, dataset_2022_tot, by = "DEN_REG") %>% 
  ggplot(aes(fill = Laureati)) +
  geom_sf(col = "black") +
  theme_void() +
  scale_fill_distiller(direction = 1) +
  # etichette
  geom_sf_text(aes(label = DEN_REG), size = 1.7)


# End #
