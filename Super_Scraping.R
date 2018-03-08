
source("fun.R")
load.lib("rvest", "dplyr", "stringr")


#****************************************************************************
## 1. Obtener enlaces F290 ####
#****************************************************************************
# Definimos la conexión
con <- url("https://www.superfinanciera.gov.co/publicacion/10427", "rb") 

# Reading the HTML code from the website
webpage <- read_html(con)
summary(webpage)
close(con)

# Obtenemos los enlaces
nodos <- html_nodes(webpage,'a')
links <- html_attr(nodos, "href")

# Obtenemos los nombres de los enlaces
reportes <- html_attr(nodos, "title")
reportes

# Obtenemos los textos de enlace
text_link<-html_text(nodos)
text_link

#******************************************
# Limpiamos (dejamos sólo los necesarios)
#******************************************
links_super<-data.frame(Mes=text_link, titulo=reportes,link=links)
links_super$link <- as.character(links_super$link)

for(i in 1:length(links_super$link)){
  links_super$link[i] <- str_replace(links_super$link[i], "https://www.superfinanciera.gov.co", "")
}

quedan <- "((^|,)([Ee]nero|[Ff]ebrero|[Mm]arzo|[Aa]bril|[Mm]ayo|[Jj]unio|[Jj]ulio|[Aa]gosto|[Ss]eptiembre|[Oo]ctubre|[Nn]oviembre|[Dd]iciembre))"
links_super<-links_super[grepl(quedan, links_super$Mes),]
head(links_super)


#******************************************************************
## 2. Descarga archivos ####
#******************************************************************

server_url<-"https://www.superfinanciera.gov.co"
download.290(server = server_url, df.links=links_super, folder="./data")







