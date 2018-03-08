#***************************************************************************************************************
#**************************************** Proyecto Formato 290 *************************************************
#***************************************************************************************************************
options(scipen=999)
source("fun.R")
load.lib("readxl", "lubridate", "dplyr", "reshape2")

#*************************************
# 1. Cargue Info en R ####
#*************************************
source("Super_Scraping.R")
read.290(years=2016)

#**************************************
# 2. Agrupa Aseguradoras ####
#**************************************
l <- ls()[grep("^f290", ls())]
Aseguradoras <- NULL
for(i in 1:length(l)){
  Aseguradoras <- c(Aseguradoras, names(get(l[i])))
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
  for(i in 1:length(l)){
    coinciden <- Aseguradoras$Aseguradoras[which(Aseguradoras$Agrupacion==gr[k])]
    agrupa <- c("FCorte", "CUENTA FORMATO")
    
    valida<-0
    if(length(coinciden)>0){
      for(j in 1:length(coinciden)){
        if(coinciden[j] %in% names(get(l[i]))) {
          agrupa <- c(agrupa, coinciden[j])
          valida <- valida+1
        }
      }
    }
    ## Si no esncuentra aseguradora en formato salta a siguiente búsqueda
    if(valida==0) next
    
    #Selecciona columnas a agrupar
    p <- get(l[i]) %>%
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

summary(grupo.290$JMALUCELLITRAVELERS)

#**************************************
# 3. Agrupa periodos x Aseguradora ####
#**************************************
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

#**************************************
# 4. Formulación indicadores ####
#**************************************
## Se formulan nuevos indicadores, como por ejemplo el de siniestralidad

Ctas_PrEmitida <- c(29000101005, 29000101010, 29000101015)

PrimasEmitidas <- p %>%
                  filter(Cuenta %in% Ctas_PrEmitida)

PrEmit <- colSums(PrimasEmitidas[,4:length(names(PrimasEmitidas))])



#**************************************
# 5. Guarda Resultados ####
#**************************************
save(serie.290, file = "./results/serie.290.RData")


