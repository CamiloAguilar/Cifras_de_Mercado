read.cutdate<-function(){
 cat("\014")
 FCorte<<-NA
 while(is.na(FCorte)){
  cat ("\n Please insert Cut Date in format (dd/mm/yyyy): \n")
  line <- readline()
  FCorte<<-as.Date.character(line, format = "%d/%m/%Y")
  if(is.na(FCorte)){
   message(" Invalid Date, Please try again!.")
  }
 }
 message("\n\n Cut Date updated to: ",FCorte, "\n\n")
}

clean.envir<-function(){
 cat("\014")
 x<-0
 cat ("\n Do you want to clean up the global environment? (Type 1 or 2): \n 1: Yes \n 2: No")
 x<-readline()
 while(!(x %in% c(1,2))==T){
  cat ("\n Do you want to clean up the global environment? (Type 1 or 2): \n 1: Yes \n 2: No")
  x<-readline()
 }
 if(x==1){
  message("Cleaning environment...")
  rm(list=ls(envir=globalenv()), envir=globalenv())
  gc()
  source("fun.R") 
  message("It's Done!")
 } else {
  message("Ok!")
 }
}

load.lib<-function(...){
 #detach("package:data.table", unload=TRUE)
 #remove.packages("data.table")
 needs<-as.vector(list(...))
 libs<-as.data.frame(installed.packages(lib.loc = NULL, priority = NULL,
                                        noCache = FALSE, fields = NULL,
                                        subarch = .Platform$r_arch))
 libs<-libs$Package
 for(i in 1:length(needs)){
  if( needs[[i]] %in% libs ){
   message("loading library ", needs[[i]])
   library(eval(needs[[i]]), character.only=TRUE)
  } else{
   message("Library ", needs[[i]], " is not installed... Installing library ", needs[i])
   install.packages(eval(needs[[i]]), dependencies = T)
   message("Loading library ", needs[[i]])
   library(eval(needs[[i]]), character.only=TRUE)
  }
 }
}

complete.dates<-function(df, meses){
      #print(names(df))
      addM<-which(as.factor(meses) %in% df$Fecha ==F)
      for(i in 1:length(addM)){
            ndf<-as.data.frame(t(c(format(meses[addM[i]], "%Y-%m-%d"),0)))
            names(ndf)<-c("Fecha","Cuenta")
            df<-rbind(df[1:addM[i]-1,],ndf,df[-(1:addM[i]-1),])
      }
      row.names(df)<-1:length(df[,1])
      names(df)<-c("Fecha","Cuenta")
      df$Cuenta<-as.numeric(as.character(df$Cuenta))
      return(df)
}

find.290 <- function(df.links, folder){
  require(stringr)
  l <- list.files(folder)
  files<-NULL
  for(j in 1:length(l)){
    n<-unlist(str_split(l[j], "\\."))[1]
    n<-unlist(str_split(n, "-"))[2]
    files<-c(files, n)
  }
  
  ## missing data
  missing.data <- df.links %>%
                  filter(!(df.links$Mes %in% files))
  already <- df.links$Mes[df.links$Mes %in% files]
  message("\n\n Following files have already been downloaded previously: ")
  for(i in 1:length(already)){
    print(as.character(already[i]))
  }
  cat ("\n Press intro to continue... \n")
  x<-readline()
  return(missing.data)
}

download.290 <- function(server, df.links, folder){
  require(dplyr)
  if(file.exists("./parameters/Meses.csv")==T){
    meses <- read.csv("./parameters/Meses.csv", header = T, sep=";", colClasses = "character")
  } 
  df.links <- find.290(df.links, folder)
  if(length(df.links[,1])>=1){
    for(i in 1:length(df.links[,1])) {
      if(file.exists("./parameters/Meses.csv")==T){
        n <- which(as.character(meses$Mes) == unlist(str_split(df.links$Mes[i], " "))[1])
        mes <- str_pad(meses$NumMes[n], 2, pad="0") 
      } else {mes <- ""}
      message("... downloading ", df.links$Mes[i])
      download.file(paste0(server_url, df.links$link[i]), 
                    destfile = paste0("./data/", unlist(str_split(df.links$Mes[i], " "))[2],
                                      str_pad(n, 2, pad="0"), "-", df.links$Mes[i], ".",
                                      unlist(str_split(as.character(df.links$link[i]), "\\."))[2]), 
                    mode="wb")
      message("\n\n")
    }
    message("\n ... ready the chicken!!!")
  } else {
    message("\n ... No hay informacion nueva disponible!!!")
  }
}

read.290 <- function(years){
  l<-list.files("./data")
  filtro<-NULL
  for(i in 1:length(l)){
    m<-str_sub(unlist(str_split(l[i], "\\."))[1], 
               start = nchar(unlist(str_split(l[i], "\\."))[1])-3,
               end = nchar(unlist(str_split(l[i], "\\."))[1]))
    filtro <- c(filtro, m)
  }
  filtrar <- which(filtro %in% years)
  
  l <- l[filtrar]
  for(i in 1:length(l)){
    message(" Reading ", l[i])
    nom <- paste0("f290_", unlist(str_split(l[i], "-"))[1])
    p <- read_excel(paste0("./data/", l[i]), skip=8, col_types="guess", n_max = 5)
    cols<-dim(p)[2]
    tipos <- c("text", "numeric", "text", rep("numeric", cols-3))
    y <- read_excel(paste0("./data/", l[i]), skip=8, col_types=tipos)
    
    if(as.numeric(str_sub(l[i], 5, 6))<12){
      FCorte <- paste("01", str_pad(as.numeric(str_sub(l[i], 5, 6))+1, 2, pad="0"), str_sub(l[i], 1, 4), sep="/")
    }else {
      FCorte <- paste("01", str_pad(1, 2, pad="0"), as.numeric(str_sub(l[i], 1, 4))+1, sep="/")
    }
    
    FCorte <- as.Date.character(FCorte, format = "%d/%m/%Y")-1
    y$FCorte <- FCorte
    assign(nom, y, envir=globalenv())
    
  }
}





