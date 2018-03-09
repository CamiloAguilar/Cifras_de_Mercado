#***************************************************************************************************************
#**************************************** Proyecto Formato 290 *************************************************
#***************************************************************************************************************
options(scipen=999)
source("fun.R")
load.lib("readxl", "lubridate", "dplyr", "reshape2")

#*********************************************************************************************************************
# 1. Cargue Info en R ####
#*********************************************************************************************************************
source("Super_Scraping.R")

formatos.290 <- read.290(years=2016:2017)
summary(formatos.290)


#*********************************************************************************************************************
# 2. Agrupa Aseguradoras ####
#*********************************************************************************************************************

Aseguradoras <- NULL
for(i in 1:length(formatos.290)){
  Aseguradoras <- c(Aseguradoras, names( formatos.290[[i]] ))
}
quitar <- c("CONCEPTO", "CUENTA FORMATO", "NOMBRE", "X__1", "FCorte")
Aseguradoras <- Aseguradoras[!(Aseguradoras %in% quitar) & !(is.na(Aseguradoras))]
Aseguradoras <- data.frame(Aseguradoras=unique(Aseguradoras))
Aseguradoras$Aseguradoras <- as.character(Aseguradoras$Aseguradoras)

Grupos <- read.table("./parameters/Aseg.Grupos.csv", header=T, sep=";")
Aseguradoras <- merge(Aseguradoras, Grupos, by="Aseguradoras", all.x = T, all.y=F) %>%
                select(-(Freq))

grupo.290 <- list()
gr <- as.character(unique(Aseguradoras$Agrupacion))
for(k in 1:length(gr)){
  grupo.290[[k]]<-list()
  cuenta<-NULL
  pos<-1
  for(i in 1:length(formatos.290)){
    coinciden <- Aseguradoras$Aseguradoras[which(Aseguradoras$Agrupacion==gr[k])]
    agrupa <- c("FCorte", "CUENTA FORMATO")
    
    valida<-0
    if(length(coinciden)>0){
      for(j in 1:length(coinciden)){
        if(coinciden[j] %in% names( formatos.290[[i]] )) {
          agrupa <- c(agrupa, coinciden[j])
          valida <- valida+1
        }
      }
    }
    ## Si no esncuentra aseguradora en formato salta a siguiente búsqueda
    if(valida==0) next
    
    #Selecciona columnas a agrupar
    p <- formatos.290[[i]] %>%
         filter(!is.na(`CUENTA FORMATO`)) %>%
         select(agrupa)
    
    p$Total<- rowSums(p[, (length(names(p))-length(coinciden)+1):length(names(p))])
    grupo.290[[k]][[pos]] <-  p
    cuenta<-c(cuenta, i)
    pos<-pos+1
  }
  names(grupo.290[[k]]) <- paste0("Mes_", cuenta)
  message("Agrupados totales para compañías del grupo ", gr[k])
}
names(grupo.290)<-gr
summary(grupo.290)

#*********************************************************************************************************************
# 3. Agrupa periodos x Aseguradora ####
#*********************************************************************************************************************
## Agrupamos resultados de cada grupo de compañías en tablas tipo serie de tiempo

cuentas.290 <- read.table("./parameters/Cuentas290.csv", header = T, sep=";", fill=T)
serie.290<-list()
for(i in 1:length(grupo.290)){
  p<-grupo.290[[i]][[1]]
  p[is.na(p)]<-0
  p <- p %>% select(FCorte, `CUENTA FORMATO`, Total)
  for(j in 2:length(grupo.290[[i]])){
    pegar<-grupo.290[[i]][[j]]
    pegar <- pegar %>% select(FCorte, `CUENTA FORMATO`, Total)
    p<-rbind(p, pegar)
    }
  p <- merge(cuentas.290, p, by.x="Cuenta", by.y = "CUENTA FORMATO", all.x=F, all.y=T) %>%
       mutate(Total=Total*signo)
  p <- p %>%
    select(FCorte, Cuenta, Nombre_cuenta=Nombre, Ramo, Total) %>%
    dcast(Cuenta + Nombre_cuenta + Ramo ~ FCorte, value.var = "Total")
  p[is.na(p)]<-0
  serie.290[[i]]<-p
  message("Agrupada en serie la compañía ",names(grupo.290)[i])
}
names(serie.290)<-names(grupo.290)
summary(serie.290)
p<-serie.290$BBVA

#*********************************************************************************************************************
# 3. Convierte f290 a mensual ####
#*********************************************************************************************************************
## Por defecto el formato 290 viene con cifras acumuladas desde enero de cada año hasta el mes del reporte.
## Este paso convierte las cifras a mensual

p <- serie.290$BBVA
periodos <- names(p)[grepl("^[20]", names(p))]
periodos <- as.Date.character(periodos, format="%Y-%m-%d")

anhos <- unique(str_sub(periodos, 1, 4))
meses<-NULL
p2<-p
for(i in 1:length(anhos)){
  mes_max <- as.character(max(periodos[year(periodos)==anhos[i]]))
  mes_min <- as.character(min(periodos[year(periodos)==anhos[i]]))
  
  maxime <- which(names(p)==mes_max)
  minime <- which(names(p)==mes_min)
  
  k <- maxime
  while(k > minime){
    p2[,k] <- p[,k]-p[,k-1]
    k <- k-1
  }
}



#*********************************************************************************************************************
# 4. Formulación indicadores ####
#*********************************************************************************************************************
## Se formulan nuevos indicadores, como por ejemplo el de siniestralidad
## Para ello se define función generadora de indicadores a partir de la interacción de diferentes cuentas contables,
## simplemente se definen las cuentas a agupar y un nombre sobre el indicador y la función genera un campo con dicha
## definición para todos los Ramos de todos los grupos aseguradores.

#************************
# Prima Emitida Directa
#************************
# Primas Emitidas Directas + Primas aceptadas en coaseguro - Cancelaciones y/o anulaciones
NombreIndicador <- "Prima Emitida Directa"
Ctas_Asociadas <- c(01005 , 01010, 01015)

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)

## Para validar la nueva característica:
summary(serie.290)
p<-serie.290$BBVA


#******************************
# Prima Aceptada en Reaseguro
#******************************
# Aceptadas Reaseguro interior + Aceptadas Reaseguro exterior - Cancelaciones y/o anulaciones

NombreIndicador <- "Prima Aceptada en Reaseguro"
Ctas_Asociadas <- c(01020, 01025, 01030)

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)

## Para validar la nueva característica:
summary(serie.290)
p<-serie.290$BBVA

#******************************
# Prima Cedida en Reaseguro
#******************************
# Cedidas Reaseguro interior + Cedidas Reaseguro exterior - Cancelaciones y/o anulaciones

NombreIndicador <- "Prima Cedida en Reaseguro"
Ctas_Asociadas <- c(01035, 01040, 01045)

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)

## Para validar la nueva característica:
summary(serie.290)
p<-serie.290$BBVA


#******************************
# Prima Cedida en Reaseguro
#******************************
# Cedidas Reaseguro interior + Cedidas Reaseguro exterior - Cancelaciones y/o anulaciones

NombreIndicador <- "Prima Cedida en Reaseguro"
Ctas_Asociadas <- c(01035, 01040, 01045)

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = FALSE)

## Para validar la nueva característica:
summary(serie.290)
p<-serie.290$BBVA



#******************************
# Índice Siniestralidad
#******************************
# Cedidas Reaseguro interior + Cedidas Reaseguro exterior - Cancelaciones y/o anulaciones

NombreIndicador <- "Siniestralidad"
# la primera posición es numerador y la segunda el denominador, mas posiciones no serán tenidas en cuenta
Ctas_Asociadas <- c(08999, 03999) 

serie.290 <- new_feature.290(serie.290 = serie.290, counts = Ctas_Asociadas, 
                             nameFeature = NombreIndicador, percentage = TRUE)

## Para validar la nueva característica:
summary(serie.290)
p<-serie.290$BBVA


#*********************************************************************************************************************
# 5. Ramos Objetivo ####
#*********************************************************************************************************************
Ramos <- as.character(unique(serie.290$BBVA$Ramo))
RamosObj.290 <- c("TRANSPORTE", "CORRIENTE DÉBIL", "HOGAR", "MANEJO", "LUCRO CESANTE",
                  "MONTAJE Y ROTURA DE MAQUINARIA", "EXEQUIAS", "ACCIDENTES PERSONALES", 
                  "VIDA GRUPO", "RESPONSABILIDAD CIVIL", "INCENDIO", "TERREMOTO", "SUSTRACCION",
                  "DESEMPLEO")

## Verifica existencia de Ramos
RamosObj.290[!(RamosObj.290 %in% Ramos)]

#*********************************************************************************************************************
# 5. Guarda Resultados ####
#*********************************************************************************************************************
save(serie.290, file = "./results/serie.290.RData")


