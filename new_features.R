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
Ctas_Asociadas <- c("01005" , "01010", "01015")

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)

## Revisamos la nueva feature
p <- serie.290$BBVA[str_sub(serie.290$BBVA$Cuenta, 7, 11) %in% c(Ctas_Asociadas, 
                                                               str_pad(sum(as.numeric(Ctas_Asociadas)), 5, pad="0")),]
write.table(p, "./validations/serie.290_PrEmitidaDirecta.csv", sep=";", row.names = F)

#********************************************************************************
# 2. Prima Aceptada en Reaseguro ####
#********************************************************************************
# Aceptadas Reaseguro interior + Aceptadas Reaseguro exterior - Cancelaciones y/o anulaciones

NombreIndicador <- "Prima Aceptada en Reaseguro"
Ctas_Asociadas <- c("01020", "01025", "01030")

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)

## Revisamos la nueva feature
p <- serie.290$CARDIF[str_sub(serie.290$CARDIF$Cuenta, 7, 11) %in% c(Ctas_Asociadas, 
                                                                     str_pad(sum(as.numeric(Ctas_Asociadas)), 5, pad="0")),]
write.table(p, "./validations/serie.290_PrReaseg.csv", sep=";", row.names = F)


#********************************************************************************
# 3. Prima Cedida en Reaseguro ####
#********************************************************************************
# Cedidas Reaseguro interior + Cedidas Reaseguro exterior - Cancelaciones y/o anulaciones

NombreIndicador <- "Prima Cedida en Reaseguro"
Ctas_Asociadas <- c("01035", "01040", "01045")

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)

## Revisamos la nueva feature
p <- serie.290$ALFA[str_sub(serie.290$ALFA$Cuenta, 7, 11) %in% c(Ctas_Asociadas, 
                                                                 str_pad(sum(as.numeric(Ctas_Asociadas)), 5, pad="0")),]
write.table(p, "./validations/serie.290_PrCedida.csv", sep=";", row.names = F)


#********************************************************************************
# 4. Índice Siniestralidad ####
#********************************************************************************
# Primas Devengadas / Siniestros cuenta compañia

NombreIndicador <- "Indice de Siniestralidad"
# la primera posición es numerador y la segunda el denominador, mas posiciones no serán tenidas en cuenta
Ctas_Asociadas <- c("08999", "03999") 

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, nameFeature = NombreIndicador, 
                             percentage = TRUE, abs.value = FALSE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, nameFeature = NombreIndicador, 
                               percentage = TRUE, abs.value = FALSE)

## Revisamos la nueva feature
p <- serie.290$METLIFE[str_sub(serie.290$METLIFE$Cuenta, 7, 11) %in% c(Ctas_Asociadas, 
                                                                 str_pad(sum(as.numeric(Ctas_Asociadas)), 5, pad="0")),]
write.table(p, "./validations/serie.290_IndiceSiniestralidad.csv", sep=";", row.names = F)


#********************************************************************************
# 5. Comisiones recibidas ####
#********************************************************************************
# Comisiones de Reaseguro Cedido + Participación Utilidades Reaseguro Cedido

NombreIndicador <- "Comisiones Recibidas"
Ctas_Asociadas <- c("09005", "09015")

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, keep=FALSE,
                             nameFeature = NombreIndicador, percentage = FALSE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, keep=FALSE,
                               nameFeature = NombreIndicador, percentage = FALSE)

## Revisamos la nueva feature
p <- serie.290$BBVA[str_sub(serie.290$BBVA$Cuenta, 7, 11) %in% c(Ctas_Asociadas, 
                                                                 str_pad(sum(as.numeric(Ctas_Asociadas)), 5, pad="0")),]
write.table(p, "./validations/serie.290_ComisionesRecibidas.csv", sep=";", row.names = F)


#********************************************************************************
# 6. Comisiones pagadas ####
#********************************************************************************
# Remuneracion intermediarios + Comisiones AFP + Remuneración uso red y canales

NombreIndicador <- "Comisiones Pagadas"
Ctas_Asociadas <- c("12005", "12010", "12025")

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, keep=FALSE,
                             nameFeature = NombreIndicador, percentage = FALSE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, keep=FALSE,
                               nameFeature = NombreIndicador, percentage = FALSE)

## Revisamos la nueva feature
p <- serie.290$BBVA[str_sub(serie.290$BBVA$Cuenta, 7, 11) %in% c(Ctas_Asociadas, 
                                                                 str_pad(sum(as.numeric(Ctas_Asociadas)), 5, pad="0")),]
write.table(p, "./validations/serie.290_ComisionesPagadas.csv", sep=";", row.names = F)


#********************************************************************************
# 7. Emitidas Directas, coaseguro y Aceptadas en Reaseguro ####
#********************************************************************************
# Emitidas directas y coaseguro (1) + Aceptadas en reaseguro (2)

NombreIndicador <- "Emitidas Directas y Aceptadas en Reaseguro"
Ctas_Asociadas <- c("03075", "03030")

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, keep=FALSE,
                             nameFeature = NombreIndicador, percentage = FALSE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, keep=FALSE,
                               nameFeature = NombreIndicador, percentage = FALSE)

## Revisamos la nueva feature
p <- serie.290$CARDIF[str_sub(serie.290$CARDIF$Cuenta, 7, 11) %in% c(Ctas_Asociadas, 
                                                                     str_pad(sum(as.numeric(Ctas_Asociadas)), 5, pad="0")),]
write.table(p, "./validations/serie.290_EmitidasyReaseg.csv", sep=";", row.names = F)


#********************************************************************************
# 8. Indice Comisiones pagadas ####
#********************************************************************************
# Comisiones pagadas / ( Emitidas directas y coaseguro (1) + Aceptadas en reaseguro (2))

NombreIndicador <- "Indice Comisiones pagadas"
# la primera posición es numerador y la segunda el denominador, mas posiciones no serán tenidas en cuenta
Ctas_Asociadas <- c("36040", "06105")

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, keep=FALSE, nameFeature = NombreIndicador, 
                             percentage = TRUE, abs.value = TRUE)
m_serie.290 <- new_feature.290(serie.290 = m_serie.290, counts = Ctas_Asociadas, keep=FALSE, nameFeature = NombreIndicador,
                               percentage = TRUE, abs.value = TRUE)

## Revisamos la nueva feature
p <- serie.290$BBVA[str_sub(serie.290$BBVA$Cuenta, 7, 11) %in% c(Ctas_Asociadas, 
                                                                 str_pad(sum(as.numeric(Ctas_Asociadas)), 5, pad="0")),]
write.table(p, "./validations/serie.290_Ind_ComiPagadas.csv", sep=";", row.names = F)


#********************************************************************************
# 99. Guarda resultados ####
#********************************************************************************
save(serie.290, file = "./results/serie.290(features).RData")
saveRDS(serie.290, file="./results/serie.290(features).rds")
write.table(serie.290$BBVA, "./validations/serie.290_BBVA.csv", sep=";", row.names = F)
write.table(serie.290$CHUBB, "./validations/serie.290_CHUBB.csv", sep=";", row.names = F)


save(m_serie.290, file = "./results/m_serie.290(features).RData")
saveRDS(m_serie.290, file="./results/m_serie.290(features).rds")
write.table(m_serie.290$BBVA, "./validations/m_serie.290_BBVA.csv", sep=";", row.names = F)
write.table(m_serie.290$CHUBB, "./validations/m_serie.290_CHUBB.csv", sep=";", row.names = F)






df <- structure(list(var1 = c(1L, 2L, 2L, 3L, 1L, 1L, 3L, 2L, 4L, 4L
), var2 = structure(c(10L, 1L, 8L, 3L, 5L, 4L, 7L, 9L, 2L, 6L
), .Label = c("b", "c", "f", "h", "i", "o", "s", "t", "w", "x"
), class = "factor"), var3 = c(7L, 5L, 5L, 8L, 5L, 8L, 6L, 7L, 
                               5L, 8L), var4 = structure(c(8L, 5L, 1L, 4L, 7L, 4L, 3L, 6L, 9L, 
                                                           2L), .Label = c("b", "c", "d", "e", "f", "h", "i", "w", "y"), 
                                                         class = "factor")), .Names = c("var1", "var2", "var3", "var4"), 
row.names = c(NA, -10L), class = "data.frame")

df

df %>% arrange(desc(var3))

vector_names <- c("var3", "var4")
vector_names <- paste0("desc(", vector_names[1],")")

df %>% arrange_(.dots = vector_names)







