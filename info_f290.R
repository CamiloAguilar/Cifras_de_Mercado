#***************************************************************************************************************
#**************************************** Proyecto Formato 290 *************************************************
#***************************************************************************************************************
options(scipen=999)
source("fun.R")
load.lib("readxl", "lubridate", "dplyr", "reshape2", "stringr")

#*********************************************************************************************************************
# 1. Cargue Info en R ####
#*********************************************************************************************************************
source("Super_Scraping.R")

years <- 2016:2017; save(years, file="./results/years.RData")
#formatos.290 <- read.290(years=years)
summary(formatos.290)


#*********************************************************************************************************************
# 2. Agrupa Aseguradoras ####
#*********************************************************************************************************************

Aseguradoras <- NULL
for(i in 1:length(formatos.290)){
  Aseguradoras <- c(Aseguradoras, names( formatos.290[[i]] ))
}
quitar <- c("CONCEPTO", "CUENTA FORMATO", "NOMBRE", "X__1", "FCorte", "COMP. DE SEGUROS GENERALES",
            "COMP. DE SEGUROS DE VIDA")
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
# 3. Serie de datos x Aseguradora ####
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
class(serie.290)<-"ts.290"
names(serie.290)<-names(grupo.290)
summary(serie.290)
p<-serie.290$BBVA
write.table(p, "./pruebas/290bbva.normal.csv", sep=";", row.names = F)

#*********************************************************************************************************************
# 3. Genera subtotales de interés ####
#*********************************************************************************************************************
## Se generan valores subtotales por compañias de Vida y gnrales, ramos objetivo o cualquier agrupación de interés
## los cuales podrán luego ser segmentados a partir de la columna 'Ramo'

Ramos <- as.character(unique(serie.290$BBVA$Ramo))

#*************************************
## Parametrización de los grupos
#*************************************
Ramos_Generales <- c("AGROPECUARIO","AUTOMOVILES","AVIACIÓN","CORRIENTE DÉBIL","CRÉDITO A LA EXPORTACIÓN",
                     "CRÉDITO COMERCIAL","CUMPLIMIENTO","EDUCATIVO","HOGAR","INCENDIO","LUCRO CESANTE",
                     "MANEJO","MINAS Y PETRÓLEOS","MONTAJE Y ROTURA DE MAQUINARIA","NAVEGACIÓN Y CASCO",
                     "RESPONSABILIDAD CIVIL","SOAT","SUSTRACCION","TERREMOTO","TODO RIESGO CONTRATISTA",
                     "TRANSPORTE","VIDRIOS")

Ramos_Vida <- c("ACCIDENTES PERSONALES","BEPS","COLECTIVO VIDA","ENFERMEDADES DE ALTO COSTO","EXEQUIAS",
                "PATRIM. AUTÓNOMO - FDO. PENS. VOLUNTARIAS","PENSIONES CON CONMUTACIÓN PENSIONAL",
                "PENSIONES LEY 100","PENSIONES VOLUNTARIAS","PREVISIONAL","RENTAS VOLUNTARIAS",
                "RIESGOS LABORALES","SALUD","VIDA GRUPO","VIDA INDIVIDUAL","DESEMPLEO")

Ramos_Objetivo <- c("TRANSPORTE", "CORRIENTE DÉBIL", "HOGAR", "MANEJO", "LUCRO CESANTE",
                  "MONTAJE Y ROTURA DE MAQUINARIA", "EXEQUIAS", "ACCIDENTES PERSONALES", 
                  "VIDA GRUPO", "RESPONSABILIDAD CIVIL", "INCENDIO", "TERREMOTO", "SUSTRACCION",
                  "DESEMPLEO", "VIDA INDIVIDUAL")

Ramos_Objetivo_Grales <- c("TRANSPORTE", "CORRIENTE DÉBIL", "HOGAR", "MANEJO", "LUCRO CESANTE",
                           "MONTAJE Y ROTURA DE MAQUINARIA", "RESPONSABILIDAD CIVIL", "INCENDIO", 
                           "TERREMOTO", "SUSTRACCION")

Ramos_SS <- c("PATRIM. AUTÓNOMO - FDO. PENS. VOLUNTARIAS","PENSIONES CON CONMUTACIÓN PENSIONAL",
              "PENSIONES LEY 100","PENSIONES VOLUNTARIAS","PREVISIONAL","RENTAS VOLUNTARIAS",
              "RIESGOS LABORALES")

Ramos_Vida_sinSS <- c("ACCIDENTES PERSONALES","BEPS","COLECTIVO VIDA","ENFERMEDADES DE ALTO COSTO","EXEQUIAS",
                      "SALUD","VIDA GRUPO","VIDA INDIVIDUAL", "DESEMPLEO")

Ramos_grupos <- list(Ramos_Vida, Ramos_Generales, Ramos_Objetivo, Ramos_Objetivo_Grales, Ramos_SS, Ramos_Vida_sinSS)
names(Ramos_grupos) <- c("Ramos_Vida", "Ramos_Generales", "Ramos_Objetivo", "Ramos_Objetivo_Grales", 
                         "Ramos_SS", "Ramos_Vida_sinSS")
saveRDS(Ramos_grupos, "./results/Ramos_grupos.rds")

numRamos <- 191:(191+length(Ramos_grupos))

#*************************************
## Cálculo subtotales para los grupos
#*************************************


for(k in 1: length(serie.290)){
  p <- serie.290[[k]]
  message("\n Preparando la serie para la compañía ",names(grupo.290)[k])
  for(i in 1:length(Ramos_grupos)){
    gr_ramo <- p %>%
               mutate(Cuenta = (str_sub(Cuenta, 7, 12))) %>%
               filter(Ramo %in% Ramos_grupos[[i]]) %>%
               select(-(Ramo)) %>%
               group_by(Cuenta, Nombre_cuenta) %>%
               summarise_all(sum) %>%
               mutate(Ramo = as.character(names(Ramos_grupos)[i])) %>%
               group_by(Cuenta, Nombre_cuenta, Ramo) %>%
               summarise_all(sum)
    gr_ramo <- as.data.frame(gr_ramo)
    gr_ramo$Cuenta <- paste0("290", numRamos[i], str_pad(gr_ramo$Cuenta, 5, pad="0"))
    
    ## Coerciona valores
    for(j in 4:length(names(gr_ramo))){
      gr_ramo[, j] <- as.numeric(gr_ramo[, j])
    }
    
    ## Guarda resultado
    p <- rbind(p, gr_ramo)
    message("...Generado subtotal ", names(Ramos_grupos)[i])
  }
  serie.290[[k]] <- p
  rm(p)
}

summary(serie.290)
p<-serie.290$BBVA
write.table(p, "./pruebas/290bbva.normal.csv", sep=";", row.names = F)




#*********************************************************************************************************************
# 4. Serie mensual ####
#*********************************************************************************************************************
## Por defecto el formato 290 viene con cifras acumuladas desde enero de cada año hasta el mes del reporte.
## Este paso convierte las cifras a mensual


m_serie.290<-list()
for(l in 1:length(grupo.290)){
  p <- serie.290[[l]]
  periodos <- names(p)[grepl("^[20]", names(p))]
  periodos <- as.Date.character(periodos, format="%Y-%m-%d")
  
  anhos <- unique(str_sub(periodos, 1, 4))
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
  m_serie.290[[l]]<-p2
  message("Calculada serie mensual para la compañía ",names(serie.290)[l])
}
class(m_serie.290)<-"ts.290"
names(m_serie.290)<-names(serie.290)
summary(m_serie.290)
p2 <- m_serie.290$BBVA
write.table(p2, "./pruebas/290bbva.mensual.csv", sep=";", row.names = F)



#*********************************************************************************************************************
# 5. Guarda Resultados ####
#*********************************************************************************************************************
save(formatos.290, file = "./results/formatos.290.RData")
save(serie.290, file = "./results/serie.290.RData")
save(m_serie.290, file = "./results/m_serie.290.RData")
save(Ramos_grupos, file = "./results/Ramos_grupos.RData")
