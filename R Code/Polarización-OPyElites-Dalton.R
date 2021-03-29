
##### CALCULOS DE POLARIZACIÓN - OPINIÓN PÚBLICA Y ÉLITES ####

library(ggplot2)

###################### Dalton Sin Partidos - OP ######################################

# Importación de bases

ImportLatinobarometro <- function (Año) {
        rio::import(here::here("Data", "Latinobarometro", 
                               paste(Año, "Latinobarometro.sav", sep="-")))}

B2018 <- ImportLatinobarometro(2018)
B2017 <- ImportLatinobarometro(2017)
B2016 <- ImportLatinobarometro(2016)
B2015 <- ImportLatinobarometro(2015)
B2013 <- ImportLatinobarometro(2013)
B2011 <- ImportLatinobarometro(2011)
B2010 <- ImportLatinobarometro(2010)
B2009 <- ImportLatinobarometro(2009)
B2008 <- ImportLatinobarometro(2008)
B2007 <- ImportLatinobarometro(2007)
B2006 <- ImportLatinobarometro(2006)
B2005 <- ImportLatinobarometro(2005)
B2004 <- ImportLatinobarometro(2004)
B2003 <- ImportLatinobarometro(2003)
B2002 <- ImportLatinobarometro(2002)
B2001 <- ImportLatinobarometro(2001)
B2000 <- ImportLatinobarometro(2000)
B1998 <- ImportLatinobarometro(1998)
B1997 <- ImportLatinobarometro(1997)
B1996 <- ImportLatinobarometro(1996)
B1995 <- ImportLatinobarometro(1995)

Base2018 <- B2018[B2018$IDENPA=="858",]
Base2017 <- B2017[B2017$IDENPA=="858",] 
Base2016 <- B2016[B2016$IDENPA=="858",]
Base2015 <- B2015[B2015$IDENPA=="858",]
Base2013 <- B2013[B2013$IDENPA=="858",]
Base2011 <- B2011[B2011$IDENPA=="858",]
Base2010 <- B2010[B2010$IDENPA=="858",]
Base2009 <- B2009[B2009$idenpa=="858",]
Base2008 <- B2008[B2008$idenpa=="858",]
Base2007 <- B2007[B2007$idenpa=="858",]
Base2006 <- B2006[B2006$idenpa=="858",]
Base2005 <- B2005[B2005$idenpa=="858",]
Base2004 <- B2004[B2004$idenpa=="858",]
Base2003 <- B2003[B2003$idenpa=="858",]
Base2002 <- B2002[B2002$idenpa=="858",]
Base2001 <- B2001[B2001$idenpa=="858",]
Base2000 <- B2000[B2000$IDENPA=="858",]
Base1998 <- B1998[B1998$idenpa=="858",]
Base1997 <- B1997[B1997$idenpa=="858",]
Base1996 <- B1996[B1996$pais=="858",]
Base1995 <- B1995[B1995$pais=="858",]

B2006$p47st

DaltonOP <- function(Base, VectorIdeologia) {
        
        # Definición de parámetro
        Vi <- (1/nrow(Base))*100
        Ri <- VectorIdeologia
        Vsum <- ((nrow(Base)-sum(is.na(VectorIdeologia)))/nrow(Base))*100
        
        # Operaciones
        MediaPnd<- sum((Ri*Vi)/Vsum, na.rm = T)
        Distancia <- (Ri-MediaPnd)/5
        Distancia2 <- Distancia^2
        
        Dalton <- (sum(Distancia2*Vi, na.rm = T))^(1/2)
        return(Dalton)
}

D18 <- DaltonOP(Base2018, Base2018$P22ST[Base2018$P22ST >=0 &Base2018$P22ST <= 10])
D17 <- DaltonOP(Base2017, Base2017$P19STC[Base2017$P19STC >=0 &Base2017$P19STC <= 10])
D16 <- DaltonOP(Base2016, Base2016$P17ST[Base2016$P17ST >=0 &Base2016$P17ST <= 10])
D15 <- DaltonOP(Base2015, Base2015$P27ST[Base2015$P27ST >=0 &Base2015$P27ST <= 10])
D13 <- DaltonOP(Base2013, Base2013$P41ST[Base2013$P41ST >=0 &Base2013$P41ST <= 10])
D11 <- DaltonOP(Base2011, Base2011$P76ST[Base2011$P76ST >=0 &Base2011$P76ST <= 10])
D10 <- DaltonOP(Base2010, Base2010$P60ST[Base2010$P60ST >=0 &Base2010$P60ST <= 10])
D09 <- DaltonOP(Base2009, Base2009$p69st[Base2009$p69st >=0 &Base2009$p69st <= 10])
D08 <- DaltonOP(Base2008, Base2008$p56st[Base2008$p56st >=0 &Base2008$p56st <= 10])
D07 <- DaltonOP(Base2007, Base2007$p67st[Base2007$p67st >=0 &Base2007$p67st <= 10])
D06 <- DaltonOP(Base2006, Base2006$p47st[Base2006$p47st >=0 &Base2006$p47st <= 10])
D05 <- DaltonOP(Base2005, Base2005$p34st[Base2005$p34st >=0 &Base2005$p34st <= 10])
D04 <- DaltonOP(Base2004, Base2004$p87st[Base2004$p87st >=0 &Base2004$p87st <= 10])
D03 <- DaltonOP(Base2003, Base2003$p60st[Base2003$p60st >=0 &Base2003$p60st <= 10])
D02 <- DaltonOP(Base2002, Base2002$p64st[Base2002$p64st >=0 &Base2002$p64st <= 10])
D01 <- DaltonOP(Base2001, Base2001$p54st[Base2001$p54st >=0 &Base2001$p54st <= 10])
D00 <- DaltonOP(Base2000, Base2000$P52ST[Base2000$P52ST >=0 &Base2000$P52ST <= 10])
D98 <- DaltonOP(Base1998, Base1998$sp52[Base1998$sp52 >=0 &Base1998$sp52 <= 10])
D97 <- DaltonOP(Base1997, Base1997$sp56[Base1997$sp56 >=0 &Base1997$sp56 <= 10])
D96 <- DaltonOP(Base1996, Base1996$p38[Base1996$p38 >=0 &Base1996$p38 <= 10])
D95 <- DaltonOP(Base1995, Base1995$p31[Base1995$p31 >=0 &Base1995$p31 <= 10])

Dalton_OP_B <- cbind(c(2018, 2017, 2016, 2015,2011, 2013, 2010, 2009, 2008, 2007,2006, 2005, 
                       2004, 2003, 2002, 2001, 2000, 1998, 1997, 1996, 1995), 
                     c(D18, D17, D16, D15, D13, D11, D10, D09, D08, D07, D06, D05, 
                       D04, D03,D02, D01, D00, D98, D97, D96, D95), rep("OP_BAROM"))




######################      Dalton OP LAPOP      ######################################


L2018 <- rio::import(here::here("Data", "LAPOP", "2018-LAPOP.dta"))
L2016 <- rio::import(here::here("Data", "LAPOP", "2016-LAPOP.dta"))
L2014 <- rio::import(here::here("Data", "LAPOP", "2014-LAPOP.sav"))
L2012 <- rio::import(here::here("Data", "LAPOP", "2012-LAPOP.sav"))
L2010 <- rio::import(here::here("Data", "LAPOP", "2010-LAPOP.sav"))
L2008 <- rio::import(here::here("Data", "LAPOP", "2008-LAPOP.sav"))
L2007 <- rio::import(here::here("Data", "LAPOP", "2007-LAPOP.sav"))


DaltonOP <- function(Base, VectorIdeologia) {
        
        # Definición de parámetro
        Vi <- (1/nrow(Base))*100
        Ri <- VectorIdeologia
        Vsum <- ((nrow(Base)-sum(is.na(VectorIdeologia)))/nrow(Base))*100
        
        # Operaciones
        MediaPnd<- sum((Ri*Vi)/Vsum, na.rm = T)
        Distancia <- (Ri-MediaPnd)/5
        Distancia2 <- Distancia^2
        
        Dalton <- (sum(Distancia2*Vi, na.rm = T))^(1/2)
        return(Dalton)
}

L18 <- DaltonOP(L2018, L2018$l1)
L16 <- DaltonOP(L2016, L2016$l1)
L14 <- DaltonOP(L2014, L2014$l1)
L12 <- DaltonOP(L2012, L2012$l1)
L10 <- DaltonOP(L2010, L2010$l1)
L08 <- DaltonOP(L2008, L2008$l1)
L07 <- DaltonOP(L2007, L2007$L1)

Dalton_OP_L <- cbind(c(2018,2016, 2014, 2012, 2010, 2008, 2007), 
                   c(L18 , L16 , L14 , L12, L10, L08, L07), rep("OP_LAPOP",7))


###################### Polarización élites PELA  ######################################


ImportPELA <- function (Año) {
        rio::import(here::here("Data", "PELA", paste(Año, "PELA-UY.sav", sep="-")))}


P2015 <- ImportPELA(2015)
P2010 <- ImportPELA(2010)
P2005 <- ImportPELA(2005)
P2000 <- ImportPELA(2000)
P1995 <- ImportPELA(1995)


DaltonELITE <- function(Base, Año) {
        
        # Subset de la base
        if (Año == 1995 | Año == 2000) {
                Base <- Base[Base$p67 <= 10,] # Eliminación de NAs
                Base <- as.data.frame(cbind(as.numeric(Base$p67), as.numeric(Base$partido)))
        } else if (Año == 2005) {
                Base <- Base[Base$p58 <= 10,] # Eliminación de NAs
                Base <- as.data.frame(cbind(as.numeric(Base$p58), as.numeric(Base$partido)))
        } else {
                Base <- Base[Base$ID1 <= 10,] # Eliminación de NAs
                Base <- as.data.frame(cbind(as.numeric(Base$ID1), as.numeric(Base$partido)))
        }
                
        # Vector resultados de resultados
        if (Año == 1995){
                Vi <- c(30.61, 31.21, 32.35, 4.93)
        } else if (Año == 2000) {
                Vi <- c(40.11, 22.31, 32.78, 4.44)
        }else if (Año == 2005) {
                Vi <- c(51.68, 35.13, 10.61, 1.88)
        }else if (Año == 2010) {
                Vi <- c(47.96, 29.07, 17.02, 2.49)
        }else if (Año == 2015) {
                Vi <- c(47.81, 30.88, 12.89)
        }else {stop("Año no disponible")}
        
        # Definición de parámetro
        
        if (Año == 2015) {
                
                Ri <- c(mean(Base[Base$V2 == "436", ]$V1, na.rm = TRUE),
                        mean(Base[Base$V2 == "438", ]$V1, na.rm = TRUE),
                        mean(Base[Base$V2 == "437", ]$V1, na.rm = TRUE))
                
        } else {
                
                Ri <- c(mean(Base[Base$V2 == "1", ]$V1, na.rm = TRUE),
                        mean(Base[Base$V2 == "2", ]$V1, na.rm = TRUE),
                        mean(Base[Base$V2 == "3", ]$V1, na.rm = TRUE),
                        mean(Base[Base$V2 == "4", ]$V1, na.rm = TRUE),
                        mean(Base[Base$V2 == "5", ]$V1, na.rm = TRUE))
        }
        
        Ri <- na.omit(Ri)
        
        # Operaciones
        MediaPnd<- sum((Ri*Vi)/sum(Vi))
        Distancia <- (Ri-MediaPnd)/4.5
        Distancia2 <- Distancia^2
        Dalton <- (sum(Distancia2*Vi))^(1/2)

        return(Dalton)
}


E15 <- DaltonELITE (P2015, 2015)
E10 <- DaltonELITE (P2010, 2010)
E05 <- DaltonELITE (P2005, 2005)
E00 <- DaltonELITE (P2000, 2000)
E95 <- DaltonELITE (P1995, 1995)



Dalton_ELITE <- cbind(c(2015, 2010, 2005, 2000, 1995), 
                      c(E15, E10, E05, E00, E95), 
                      rep("Elite",5))


Dalton <- rbind (Dalton_OP_L, Dalton_ELITE, Dalton_OP_B)
Dalton <- as.data.frame(Dalton)
names(Dalton) <- c("Year", "Pol.index", "Type")
Dalton$Pol.index <- as.numeric(as.character(Dalton$Pol.index))
Dalton$Pol.index <- round(as.numeric(Dalton$Pol.index), 2)
        
ggplot(Dalton, aes(x=Year, y=Pol.index, group = Type, colour =Type )) + 
        geom_line()  + 
        geom_point( size=2, shape=21, fill="white") +
        theme_minimal()








