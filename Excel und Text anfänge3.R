if (!require(data.table)) {
  install.packages("data.table")
  require(readxl)
}
if (!require(readr)) {
  install.packages("readr")
  require(readxl)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  require(readxl)
}
if (!require(tidyr)) {
  install.packages("tidyr")
  require(readxl)
}
if (!require(purrr)) {
  install.packages("purrr")
  require(readxl)
}
if (!require(stringr)) {
  install.packages("stringr")
  require(readxl)
}
if (!require(rebus)) {
  install.packages("rebus")
  require(readxl)
}
library(tidyverse)

#Bkomponente sind Bestanteile_Komponente, 
BKomponente10 <- fread("Bestandteile_Komponente_K1BE1.csv")
BKomponente10$V1 <- NULL
colnames(Komponente10) <- c("ID_T1", "ID_T2", "ID_T3", "ID_T4", "ID_K1BE1")
BKomponente10 <- BKomponente10[-1]
head(BKomponente10)


BKomponente11 <- fread("Bestandteile_Komponente_K1BE2.csv")
BKomponente11$V1 <- NULL
colnames(BKomponente11) <- c("ID_T1", "ID_T2", "ID_T7", "ID_T8", "ID_K1BE2")
BKomponente11 <- BKomponente11[-1]
head(BKomponente11)

BKomponente12 <- fread("Bestandteile_Komponente_K1DI1.csv")
BKomponente12$V1 <- NULL
colnames(BKomponente12) <- c("ID_T1", "ID_T2", "ID_T5", "ID_T6", "ID_K1DI1")
head(BKomponente12)

BKomponente13 <- fread("Bestandteile_Komponente_K1DI2.csv")
BKomponente13$V1 <- NULL
colnames(BKomponente13) <- c("ID_T1", "ID_T2", "ID_T9", "ID_T10", "ID_K1DI2")
BKomponente13 <- BKomponente13[-1]
head(BKomponente13)

BKomponente14 <- fread("Bestandteile_Komponente_K2LE1.csv")
BKomponente14$V1 <- NULL
head(BKomponente14)

BKomponente15 <- fread("Bestandteile_Komponente_K2LE2.csv")
BKomponente15$V1 <- NULL
head(BKomponente15)

BKomponente16 <- fread("Bestandteile_Komponente_K2ST1.csv")
BKomponente16$V1 <- NULL
BKomponente16$X1 <- NULL
head(BKomponente16)

BKomponente17 <- fread("Bestandteile_Komponente_K2ST2.csv")
BKomponente17$V1 <- NULL
head(BKomponente17)

BKomponente18 <- fread("Bestandteile_Komponente_K3AG1.csv")
BKomponente18$V1 <- NULL
head(BKomponente18)

BKomponente19 <- fread("Bestandteile_Komponente_K3AG2.csv")
BKomponente19$V1 <- NULL
head(BKomponente19)

BKomponente20 <- fread("Bestandteile_Komponente_K3SG1.csv")
BKomponente20$V1 <- NULL
head(BKomponente20)

BKomponente21 <- fread("Bestandteile_Komponente_K3SG2.csv")
BKomponente21$V1 <- NULL
head(BKomponente21)

BKomponente22 <- fread("Bestandteile_Komponente_K4.csv")
BKomponente22$V1 <- NULL
head(BKomponente22)

BKomponente23 <- fread("Bestandteile_Komponente_K5.csv")
BKomponente23$V1 <- NULL
head(BKomponente23)

BKomponente24 <- fread("Bestandteile_Komponente_K6.csv")
BKomponente24$V1 <- NULL
head(BKomponente24)

BKomponente25 <- fread("Bestandteile_Komponente_K7.csv")
BKomponente25$V1 <- NULL
head(BKomponente25)

Komponente26 <- fread("Komponente_K1BE1.csv")
BKomponente25$V1 <- NULL
BKomponente25$X1 <- NULL
Komponente26 <- transform(Komponente26, Produktionsdatum = Komponente26$Produktionsdatum_Origin_01011970 + Komponente26$origin)
Komponente26 <- Komponente26[,-(9:10),drop=FALSE]
head(Komponente26)

Komponente27 <- fread("Komponente_K1BE2.csv")
BKomponente27$V1 <- NULL
BKomponente27$X1 <- NULL
Komponente27 <- transform(Komponente27, Produktionsdatum = Komponente27$Produktionsdatum_Origin_01011970 + Komponente27$origin)
Komponente27 <- Komponente26[,-(9:10),drop=FALSE]
head(Komponente27)

Komponente28 <- fread("Komponente_K1DI1.csv")
BKomponente28$V1 <- NULL
BKomponente28$X1 <- NULL
BKomponente28 <- BKomponente28 [1:9]
head(Komponente28)

Komponente29 <- read_file("Komponente_K1DI2.txt")
Komponente29<-str_split(Komponente29,pattern="\\\"[0-9]+\"\\\\[0-9]+\\\\\\\"")
Komponente29 <- data.frame(Komponente29)
Komponente29 <- separate(Komponente29, col=1, sep = "\\\\", into = c("X1","ID_Motor","Herstellernummer","Werksnummer","Fehlerhaft","Fehlerhaft_Datum","Fehlerhaft_Fahrleistung","Produktionsdatum_Origin_01011970","origin"))
head(Komponente29)
Komponente29 <- Komponente29[-1,]
head(Komponente29)


Komponente30 <-read_file("Komponente_K2LE1.txt")
Komponente30<-str_split(Komponente30, pattern="")
Komponente30<-data.frame(Komponente30)
Komponente30<-separate(Komponente30, col = 1, sep = "II", into = c("X1","ID_Sitze.x","Herstellernummer.x","Produktionsdatum.x","Werksnummer.x","Fehlerhaft.x","Fehlerhaft_Datum.x","Fehlerhaft_Fahrleistung.x"))
head(Komponente30)

Komponente31 <- read_file("Komponente_K2LE2.txt")
Komponente31<-str_split(Komponente31,pattern="\\\"[0-9]+\"\\\\[0-9]+\\\\\\\"")
Komponente31 <- data.frame(Komponente31)
head(Komponente31)

Komponente32 <- fread("Komponente_K2ST1.txt")
BKomponente32$V1 <- NULL
BKomponente32$X1 <- NULL
head(Komponente32)

Komponente33 <- fread("Komponente_K2ST2.csv")
head(Komponente33)

Komponente34 <- fread("Komponente_K3AG1.csv")
head(Komponente34)

Komponente35 <- read_file("Komponente_K3AG2.txt")
Komponente35 <- str_split(Komponente35,pattern="\\\"[0-9]+\"\\\\[0-9]+\\\\\\\"")
Komponente35 <- data.frame(Komponente35)
Komponente35<-separate(Komponente35, col = 1, sep = "\\\\", into = c("X1","ID_Schaltung","Herstellernummer","Werksnummer","Fehlerhaft","Fehlerhaft_Datum","Fehlerhaft_Fahrleistung","Produktionsdatum_Origin_01011970","origin"))
head(Komponente35)


Komponente36 <- fread("Komponente_K3SG1.csv")
BKomponente32$V1 <- NULL
BKomponente32$X1 <- NULL
BKomponente36 <- BKomponente36 [1:9]
head(Komponente36)

Komponente37 <- fread("Komponente_K3SG2.csv")
Komponente37 <- transform(Komponente37, Produktionsdatum = Komponente37$Produktionsdatum_Origin_01011970 + Komponente37$origin)
Komponente37 <- Komponente26[,-(9:10),drop=FALSE]
head(Komponente37)

Komponente38 <- fread("Komponente_K4.csv")
head(Komponente38)

Komponente39 <- fread("Komponente_K5.csv")

head(Komponente39)

Komponente40 <- fread("Komponente_K6.csv")
BKomponente32$V1 <- NULL
BKomponente32$X1 <- NULL
head(Komponente40)

Komponente41 <- fread("Komponente_K7.txt")
BKomponente32$V1 <- NULL
BKomponente32$X1 <- NULL
Komponente37 <- transform(Komponente37, Produktionsdatum = Komponente37$Produktionsdatum_Origin_01011970 + Komponente37$origin)
Komponente37 <- Komponente26[,-(9:10),drop=FALSE]
head(Komponente41)

Zulassung <- fread("Zulassungen_alle_Fahrzeuge.csv")

