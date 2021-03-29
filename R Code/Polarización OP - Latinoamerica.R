### Polarización latinoamerica

# Argentina ="32"
# Brasil ="76"
# Chile = "152"
# Costa Rica = "188"
# Venezuela = "862"
# Paraguay = "600"
# Mexico = "484"
# Perú = "604"
# Uruguay = "858"

pais <- "858"

Base2018 <- B2018[B2018$IDENPA==pais,]
Base2017 <- B2017[B2017$IDENPA==pais,] 
Base2016 <- B2016[B2016$IDENPA==pais,]
Base2015 <- B2015[B2015$IDENPA==pais,]
Base2013 <- B2013[B2013$IDENPA==pais,]
Base2011 <- B2011[B2011$IDENPA==pais,]
Base2010 <- B2010[B2010$IDENPA==pais,]
Base2009 <- B2009[B2009$idenpa==pais,]
Base2008 <- B2008[B2008$idenpa==pais,]
Base2007 <- B2007[B2007$idenpa==pais,]
Base2006 <- B2006[B2006$idenpa==pais,]
Base2005 <- B2005[B2005$idenpa==pais,]
Base2004 <- B2004[B2004$idenpa==pais,]
Base2003 <- B2003[B2003$idenpa==pais,]
Base2002 <- B2002[B2002$idenpa==pais,]
Base2001 <- B2001[B2001$idenpa==pais,]
Base2000 <- B2000[B2000$IDENPA==pais,]
Base1998 <- B1998[B1998$idenpa==pais,]
Base1997 <- B1997[B1997$idenpa==pais,]
Base1996 <- B1996[B1996$pais==pais,]
Base1995 <- B1995[B1995$pais==pais,]


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


Dalton_OP_URU <- cbind(c(2018, 2017, 2016, 2015,2011, 2013, 2010, 2009, 2008, 2007,2006, 2005, 
                       2004, 2003, 2002, 2001, 2000, 1998, 1997, 1996, 1995), 
                     c(D18, D17, D16, D15, D13, D11, D10, D09, D08, D07, D06, D05, 
                       D04, D03,D02, D01, D00, D98, D97, D96, D95), rep("Uruguay"))





Dalton_OP_ARG <- as.data.frame(Dalton_OP_ARG)
Dalton_OP_BRA <- as.data.frame(Dalton_OP_BRA)
Dalton_OP_CHI <- as.data.frame(Dalton_OP_CHI)
Dalton_OP_CRI <- as.data.frame(Dalton_OP_CRI)
Dalton_OP_VEN <- as.data.frame(Dalton_OP_VEN)
Dalton_OP_PAR <- as.data.frame(Dalton_OP_PAR)
Dalton_OP_MEX <- as.data.frame(Dalton_OP_MEX)
Dalton_OP_PER <- as.data.frame(Dalton_OP_PER)
Dalton_OP_URU <- as.data.frame(Dalton_OP_URU)

PolOPLA <- full_join(Dalton_OP_ARG, Dalton_OP_BRA) %>% 
                full_join(., Dalton_OP_CHI) %>% 
                full_join(., Dalton_OP_CRI) %>% 
                full_join(., Dalton_OP_VEN) %>%
                full_join(., Dalton_OP_PAR) %>% 
                full_join(., Dalton_OP_MEX) %>% 
                full_join(., Dalton_OP_PER) %>% 
                full_join(., Dalton_OP_URU)
        
PolOPLA$V2 <- as.numeric(as.character(PolOPLA$V2))

ggplot(PolOPLA, aes(x=V1, y=V2, group = V3, colour =V3 )) + 
        geom_line()  + 
        geom_point( size=2, shape=21, fill="white") +
        theme_minimal()


