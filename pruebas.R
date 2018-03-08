

grupo.290$SURA

p<-grupo.290$SURA[[1]]
p[is.na(p)]<-0
for(i in 2:length(grupo.290$SURA)){
  p<-rbind(p, grupo.290$SURA[[i]])
}

p <- p %>%
     select(FCorte, `CUENTA FORMATO`, Total) %>%
     dcast(`CUENTA FORMATO` ~ FCorte, value.var = "Total")
p[is.na(p)]<-0


