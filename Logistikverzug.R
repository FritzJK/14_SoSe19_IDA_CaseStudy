
if( !require(tidyverse)){
  install.packages("tidyverse")
}
library(tidyverse)

if( !require(knitr)){
  install.packages("knitr")
}
library(knitr)
if( !require(readxl)){
  install.packages("readxl")
}
library(readxl)
if( !require(data.table)){
  install.packages("data.table")
}
library(data.table)

if( !require(ggplot2)){
  install.packages("ggplot2")
  require(readxl)
}


Verzug <- fread("Logistikverzug_K7.csv")
k7 <- fread("Komponente_K7.csv")

Verzug$Wareneingang <- as.Date(Verzug$Wareneingang, format = "%d.%m.%Y")
k7$Produktionsdatum <- as.Date(k7$Produktionsdatum, format = "%Y-%m-%d")

Dif <- Verzug$Wareneingang - k7$Produktionsdatum
head(Dif)

Difmin <- min(Dif)
Difmin
Difmax <- max(Dif)
Difmax
Difmean <- mean (Dif)
Difmean

Dif <- as.numeric(Dif)
Difframe <- data.frame(Dif, Verzug$Werksnummer, Verzug$Herstellernummer, Verzug$Wareneingang)
str(Difframe)
head(Difframe)
tail(Difframe)
#Fahrtdauer gegen Werksnummer
p1 <- ggplot(data= Difframe, aes(y=Dif, x = Verzug.Werksnummer)) + geom_point()
p1
#plus Mittelwert
p1.5 <- p1 + geom_hline(yintercept = Difmean)
p1.5
#einzige Info Komponente wird von 2 Werken bezogen, 2 Hersteller, datenset teilen?
p2 <- ggplot(data= Difframe, aes(y=Dif, x = Verzug.Herstellernummer)) + geom_point()
p2

p3 <- ggplot(data= Difframe, aes(y=Dif, x = Verzug.Wareneingang)) + geom_point()
p3
#Correlation Coefficient
w <- Verzug$Werksnummer
cor(Dif, w)
# -0.001427836

a <-  k7$Prdouktionsdatum
a <-as.numeric(a)
cor(Dif, a)
#0

v <-  Verzug$Wareneingang
v <-as.numeric(v)
cor(Dif, v)
#0.002822053

f <- Verzug$Fehlerhaft
cor(f, Dif)
#check if they are parts  which are broken
Verzug[Fehlerhaft=="1"]
#empty, non are

#abhängigkeit von Jahrezeit, Monat?