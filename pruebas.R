

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

## Uso de formulas

a<-1
x<-2
m<-3

my.formula <- y ~ x1 + x2

all.vars(my.formula) 
all.names(my.formula) 

a[[3]]

str_split(as.character(a[[3]]), "*")

a<-data.frame(col1=1:2, col2=3:4)
a


apply(a, MARGIN = 2, FUN = function(r){abs(r[1]/r[2])})





