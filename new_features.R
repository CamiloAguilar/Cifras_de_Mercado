#*********************************************************************************************************************
#******************************************* Nuevas Características **************************************************
#*********************************************************************************************************************
## Se formulan nuevos indicadores, como por ejemplo el de siniestralidad
## Para ello se define función generadora de indicadores a partir de la interacción de diferentes cuentas contables,
## simplemente se definen las cuentas a agupar y un nombre sobre el indicador y la función genera un campo con dicha
## definición para todos los Ramos de todos los grupos aseguradores.

## La nueva cuenta contable será el resultado de concatenar '290999' y la suma de las 'Ctas_Asociadas',
## permitiendo asi que los resultados de las nuevas características puedan ser usadas para formular nuevas
## características

options(scipen=999)
source("fun.R")
load.lib("readxl", "lubridate", "dplyr", "reshape2", "stringr")

load("./results/serie.290.RData")
load("./results/m_serie.290.RData")

#********************************************************************************
# 1. Prima Emitida Directa ####
#********************************************************************************
# Primas Emitidas Directas + Primas aceptadas en coaseguro - Cancelaciones y/o anulaciones
NombreIndicador <- "Prima Emitida Directa"
Ctas_Asociadas <- c(01005 , 01010, 01015)

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)



#********************************************************************************
# 2. Prima Aceptada en Reaseguro ####
#********************************************************************************
# Aceptadas Reaseguro interior + Aceptadas Reaseguro exterior - Cancelaciones y/o anulaciones

NombreIndicador <- "Prima Aceptada en Reaseguro"
Ctas_Asociadas <- c(01020, 01025, 01030)

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)

p<-m_serie.290$BBVA

#********************************************************************************
# 3. Prima Cedida en Reaseguro ####
#********************************************************************************
# Cedidas Reaseguro interior + Cedidas Reaseguro exterior - Cancelaciones y/o anulaciones

NombreIndicador <- "Prima Cedida en Reaseguro"
Ctas_Asociadas <- c(01035, 01040, 01045)

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)



#********************************************************************************
# 4. Índice Siniestralidad ####
#********************************************************************************
# Primas Devengadas / Siniestros cuenta compañia

NombreIndicador <- "Indice de Siniestralidad"
# la primera posición es numerador y la segunda el denominador, mas posiciones no serán tenidas en cuenta
Ctas_Asociadas <- c(08999, 03999) 

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = TRUE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, 
                               nameFeature = NombreIndicador, percentage = TRUE)


#********************************************************************************
# 5. Comisiones recibidas ####
#********************************************************************************
# Comisiones de Reaseguro Cedido + Participación Utilidades Reaseguro Cedido

NombreIndicador <- "Comisiones Recibidas"
Ctas_Asociadas <- c(09005, 09015)

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, keep=FALSE,
                             nameFeature = NombreIndicador, percentage = FALSE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, keep=FALSE,
                               nameFeature = NombreIndicador, percentage = FALSE)

#********************************************************************************
# 6. Comisiones pagadas ####
#********************************************************************************
# Remuneracion intermediarios + Comisiones AFP + Remuneración uso red y canales

NombreIndicador <- "Comisiones Pagadas"
Ctas_Asociadas <- c(12005, 12010, 12025)

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, keep=FALSE,
                             nameFeature = NombreIndicador, percentage = FALSE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, keep=FALSE,
                               nameFeature = NombreIndicador, percentage = FALSE)



#********************************************************************************
# 7. Emitidas Directas, coaseguro y Aceptadas en Reaseguro ####
#********************************************************************************
# Emitidas directas y coaseguro (1) + Aceptadas en reaseguro (2)

NombreIndicador <- "Emitidas Directas y Aceptadas en Reaseguro"
Ctas_Asociadas <- c(03075, 03030)

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, keep=FALSE,
                             nameFeature = NombreIndicador, percentage = FALSE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, keep=FALSE,
                               nameFeature = NombreIndicador, percentage = FALSE)



#********************************************************************************
# 8. Indice Comisiones pagadas ####
#********************************************************************************
# Comisiones pagadas / ( Emitidas directas y coaseguro (1) + Aceptadas en reaseguro (2))

NombreIndicador <- "Indice Comisiones pagadas"
# la primera posición es numerador y la segunda el denominador, mas posiciones no serán tenidas en cuenta
Ctas_Asociadas <- c(36040, 06105)

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, keep=FALSE,
                             nameFeature = NombreIndicador, percentage = TRUE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, keep=FALSE,
                               nameFeature = NombreIndicador, percentage = TRUE)

# revisa indicadores
p<-serie.290$BBVA
write.table(p, "p.csv", sep=";", row.names = F)


#********************************************************************************
# 99. Guarda resultados ####
#********************************************************************************
save(serie.290, file = "./results/serie.290(features).RData")
save(m_serie.290, file = "./results/m_serie.290(features).RData")


