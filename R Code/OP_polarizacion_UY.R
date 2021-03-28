# Calculo de polarización de la opinión pública


# Importación de bases

B2015 <- rio::import(here::here("Data", "Latinobarometro", "2015-Latinobarometro.sav"))
B2010 <- rio::import(here::here("Data", "Latinobarometro", "2010-Latinobarometro.sav"))
B2005 <- rio::import(here::here("Data", "Latinobarometro", "2005-Latinobarometro.sav"))
B1998 <- rio::import(here::here("Data", "Latinobarometro", "1998-Latinobarometro.sav"))
B1995 <- rio::import(here::here("Data", "Latinobarometro", "1995-Latinobarometro.sav"))

#Filtro para Uruguay
Base2015 <- B2015[B2015$IDENPA=="858",]
Base2010 <- B2010[B2010$IDENPA=="858",]
Base2005 <- B2005[B2005$idenpa=="858",]
Base1998 <- B1998[B1998$idenpa=="858",]
Base1995 <- B1995[B1995$pais=="858",]

################## Calculo de Dalton Pol index --- AÑO 2015 ###############################

Base2015 <- as.data.frame(cbind(as.numeric(Base2015$P27ST), as.numeric(Base2015$P23TGBSM)))
Ri <- c(mean(Base2015[Base2015$V2 == "858001", ]$V1, na.rm = TRUE),
        mean(Base2015[Base2015$V2 == "858002", ]$V1, na.rm = TRUE),
        mean(Base2015[Base2015$V2 == "858003", ]$V1, na.rm = TRUE),
        mean(Base2015[Base2015$V2 == "858004", ]$V1, na.rm = TRUE),
        mean(Base2015[Base2015$V2 == "858005", ]$V1, na.rm = TRUE))

# Vector de votos
# 858001=FA, 858002=PN, 858003=PC, 858004=PI, 858005=AP

Vi <- (c(47.81, 30.88, 12.89, 3.09, 1.13))

# Procedimiento
MediaPnd<- sum((Ri*Vi)/sum(Vi))
Distancia <- (Ri-MediaPnd)/5
Distancia2 <- Distancia^2
Dalton2015 <- (sum(Distancia2*Vi))^(1/2)


##########################################################################################


################## Calculo de Dalton Pol index --- AÑO 2010 ##############################

Base2010 <- as.data.frame(cbind(as.numeric(Base2010$P60ST), as.numeric(Base2010$P29ST)))
Ri <- c(mean(Base2010[Base2010$V2 == "1601", ]$V1, na.rm = TRUE),
        mean(Base2010[Base2010$V2 == "1602", ]$V1, na.rm = TRUE),
        mean(Base2010[Base2010$V2 == "1603", ]$V1, na.rm = TRUE),
        mean(Base2010[Base2010$V2 == "1604", ]$V1, na.rm = TRUE),
        mean(Base2010[Base2010$V2 == "1605", ]$V1, na.rm = TRUE))


# Vector de votos
# 1601=FA, 1602=PN, 1603=PC, 1604=PI, 1605=AP

Vi <- (c(47.96, 29.07, 17.02, 2.49, 0.67))

# Procedimiento
MediaPnd<- sum((Ri*Vi)/sum(Vi))
Distancia <- (Ri-MediaPnd)/5
Distancia2 <- Distancia^2
Dalton2010 <- (sum(Distancia2*Vi))^(1/2)

##########################################################################################


################## Calculo de Dalton Pol index --- AÑO 2005 ##############################


Base2005 <- as.data.frame(cbind(as.numeric(Base2005$p34st), as.numeric(Base2005$p48st)))
Ri <- c(mean(Base2005[Base2005$V2 == "858001", ]$V1, na.rm = TRUE),
        mean(Base2005[Base2005$V2 == "858002", ]$V1, na.rm = TRUE),
        mean(Base2005[Base2005$V2 == "858003", ]$V1, na.rm = TRUE))


# Vector de votos
# 858001=FA, 858002=PN, 858003=PC

Vi <- c(51.68, 35.13, 10.61)

# Procedimiento
MediaPnd<- sum((Ri*Vi)/sum(Vi))
Distancia <- (Ri-MediaPnd)/5
Distancia2 <- Distancia^2
Dalton2005 <- (sum(Distancia2*Vi))^(1/2)

##########################################################################################


################## Calculo de Dalton Pol index --- AÑO 2000 ##############################

Base1998 <- as.data.frame(cbind(as.numeric(Base1998$sp52), as.numeric(Base1998$sp53)))
Ri <- c(mean(Base1998[Base1998$V2 == "858007", ]$V1, na.rm = TRUE),
        mean(Base1998[Base1998$V2 == "858002", ]$V1, na.rm = TRUE),
        mean(Base1998[Base1998$V2 == "858003", ]$V1, na.rm = TRUE),
        mean(Base1998[Base1998$V2 == "858006", ]$V1, na.rm = TRUE))



# Vector de votos
# 858007=FA, 858002=PN, 858003=PC, 858006=NE

Vi <- c(40.11, 22.31, 32.78, 4.44)

# Procedimiento
MediaPnd<- sum((Ri*Vi)/sum(Vi))
Distancia <- (Ri-MediaPnd)/5
Distancia2 <- Distancia^2
Dalton1998 <- (sum(Distancia2*Vi))^(1/2)

##########################################################################################


################## Calculo de Dalton Pol index --- AÑO 1995 ##############################


Base1995 <- as.data.frame(cbind(as.numeric(Base1995$p31), as.numeric(Base1995$p33)))
Ri <- c(mean(Base1995[Base1995$V2 == "858001", ]$V1, na.rm = TRUE),
        mean(Base1995[Base1995$V2 == "858002", ]$V1, na.rm = TRUE),
        mean(Base1995[Base1995$V2 == "858003", ]$V1, na.rm = TRUE),
        mean(Base1995[Base1995$V2 == "858006", ]$V1, na.rm = TRUE))



# Vector de votos
# 858007=FA, 858002=PN, 858003=PC, 858006=NE

Vi <- c(30.61, 31.21, 32.35, 4.93)

# Procedimiento
MediaPnd<- sum((Ri*Vi)/sum(Vi))
Distancia <- (Ri-MediaPnd)/5
Distancia2 <- Distancia^2
Dalton1995 <- (sum(Distancia2*Vi))^(1/2)

##########################################################################################


################## Graficación ##############################

Año <- c(2015, 2010, 2005, 2000, 1998, 1995)
PolOP <- c(Dalton2015, Dalton2010, Dalton2005, Dalton2000, Dalton1998, Dalton1995)

PolOP <- cbind(Año, PolOP)

plot(PolOP, ylim = c(0,5))
lines(PolOP)

############################################################






