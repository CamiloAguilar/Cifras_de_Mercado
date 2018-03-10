#*********************************************************************************************************************
#*******************************************     Genera Reporte     **************************************************
#*********************************************************************************************************************

## Estadísticas de la Superintendencia con respecto a reclamaciones en compañías No Vida (Ranking y Número).

options(scipen=999)
source("fun.R")
load.lib("readxl", "lubridate", "dplyr", "reshape2", "stringr")

load("./results/serie.290(features).RData")
load("./results/m_serie.290(features).RData")
load("./results/years.RData")


#********************************************************************************
# 1. Ranking Ramos Generales (ANUAL) ####
#********************************************************************************

## Prima Emitida directa - Feature 03030
## Siniestros cuenta compañía - Feature 03999
## Siniestro Pagados - Feature 05005
## Resultado Técnico - Feature 14999
## Resultado del ejercicio - Feature 18999
## Indice de siniestralidad - Feature 12998

features <- c(03030, 03999, 05005, 14999, 18999, 12998)
gr_ramo <- c("Ramos_Generales")

rank_GRALES <- ranking.290(serie.290 = serie.290, gr_ramo = gr_ramo, features = features, years = years)
summary(rank_GRALES)

View(rank_GRALES$P2016)
copy.table(rank_GRALES$P2016)

View(rank_GRALES$P2017)
copy.table(rank_GRALES$P2017)

#********************************************************************************
# 2. Ranking Ramos Generales (Objetivo) (ANUAL) ####
#********************************************************************************
## Prima Emitida directa - Feature 03030
## Siniestros cuenta compañía - Feature 03999
## Siniestro Pagados - Feature 05005
## Resultado Técnico - Feature 14999
## Resultado del ejercicio - Feature 18999
## Indice de siniestralidad - Feature 12998


features <- c(03030, 03999, 05005, 14999, 18999, 12998)
gr_ramo <- c("Ramos_Objetivo_Grales")

rank_GRALES_obj <- ranking.290(serie.290 = serie.290, gr_ramo = gr_ramo, features = features, years = years)
summary(rank_GRALES_obj)

View(rank_GRALES_obj$P2016)
copy.table(rank_GRALES_obj$P2016)

View(rank_GRALES_obj$P2017)
copy.table(rank_GRALES_obj$P2017)


#********************************************************************************
# 2. POR RAMO OBJETIVO ####
#********************************************************************************
## Prima Emitida directa - Feature 03030
## Siniestros cuenta compañía - Feature 03999
## Siniestro Pagados - Feature 05005
## Resultado Técnico - Feature 14999
## Resultado del ejercicio - Feature 18999
## Indice de siniestralidad - Feature 12998

Ramos_Objetivo_Grales

features <- c(03030, 03999, 05005, 14999, 18999, 12998)
gr_ramo <- c("SUSTRACCION")

rank_RAMO_obj <- ranking.290(serie.290 = serie.290, gr_ramo = gr_ramo, features = features, years = years)
summary(rank_RAMO_obj)

copy.table(rank_RAMO_obj$P2016)
copy.table(rank_RAMO_obj$P2017)






