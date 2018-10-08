rm(list=ls())
library(dplyr)
library(Hmisc)
library(lubridate)
setwd("D:/Documents/GitHub/Trabajo2")
db <- read.csv2("Autos TRM.csv",header = TRUE,stringsAsFactors = FALSE)
db$Fecha <- as.Date(db$Fecha, format = "%d/%m/%Y") #Formating the date
db$TRM=gsub("\\$", "", db$TRM)
db$TRM=gsub("\\.", "", db$TRM)
db$TRM=as.numeric(gsub("\\,", ".", db$TRM))
db<-cbind2(db,month(db$Fecha))
db<-cbind2(db,weekdays(db$Fecha))
names(db)[4]<-"mes"
names(db)[5]<-"dia"
#Creación de dataframe por meses
attach(db)
dbmeses<-db%>%group_by(Mes=floor_date(Fecha,"month"))%>%summarise(Unidades=sum(Unidades),TRM=mean(TRM))
dbmeses$Mes<-as.Date(dbmeses$Mes)
googleaux<-read.csv2("multiTimeline (1).csv",header=T,sep=",")
dbmeses<-cbind2(dbmeses,googleaux$carro...Colombia.)
names(dbmeses)[2]<-"uni"
names(dbmeses)[4]<-"pop"
names(dbmeses)[1]<-"mes"
#Incluir popularidad en el dataframe por día
vpop<-rep(dbmeses$pop[1],monthDays(dbmeses$mes[1]))
for (i in 2:nrow(dbmeses)){
  auxpop<-rep(dbmeses$pop[i],monthDays(dbmeses$mes[i]))
  vpop<-append(vpop,auxpop)
}
db<-cbind2(db,vpop)
names(db)[6]<-"pop"
empleoaux<-read.csv2("Empleo_desempleo.csv",header=FALSE,stringsAsFactors = FALSE)
names(empleoaux)[1]<-"fecha"
names(empleoaux)[2]<-"templeo"
names(empleoaux)[3]<-"tdesem"
empleoaux$fecha<-as.Date(paste(empleoaux$fecha,1,sep="-"),"%Y-%m-%d")
empleoaux<-empleoaux[order(empleoaux$fecha),]
dbmeses<-cbind2(dbmeses,empleoaux$templeo)
dbmeses<-cbind2(dbmeses,empleoaux$tdesem)
names(dbmeses)[5]<-"templeo"
names(dbmeses)[6]<-"tdesem"

#Incluir tasa de empleo
vtemp<-rep(empleoaux$templeo[1],monthDays(empleoaux$fecha[1]))
for (i in 2:nrow(empleoaux)){
  auxtemp<-rep(empleoaux$templeo[i],monthDays(empleoaux$fecha[i]))
  vtemp<-append(vtemp,auxtemp)
}
db<-cbind2(db,vtemp)
names(db)[7]<-"templeo"

#Incluir tasa de desempleo
vtdesem<-rep(empleoaux$tdesem[1],monthDays(empleoaux$fecha[1]))
for (i in 2:nrow(empleoaux)){
  auxtdesem<-rep(empleoaux$tdesem[i],monthDays(empleoaux$fecha[i]))
  vtdesem<-append(vtdesem,auxtdesem)
}
db<-cbind2(db,vtdesem)
names(db)[8]<-"tdesem"

#Analizar los datos por día de la semana
semanacompra<-db%>%group_by(dia)%>%summarise(Unidades=sum(Unidades))
semanacompra$dia <- factor(semanacompra$dia, levels= c("lunes", "martes","miércoles", "jueves", "viernes", "sábado", "domingo"))
semanacompra<-semanacompra[order(semanacompra$dia), ]

#Análisis por día de la semana
yy=barplot(semanacompra$Unidades)
axis(1, at=yy,labels=semanacompra$dia, tick=FALSE, las=2, line=-0.5)

#Analizar los datos por mes
mescompra<-db%>%group_by(mes)%>%summarise(Unidades=sum(Unidades))

#Análisis de meses y compras
xx=barplot(mescompra$Unidades)
axis(1, at=yy,labels=c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"), tick=FALSE, las=2, line=-0.5, cex.axis=0.9)

#Análisis del dólar y de los carros registrados por meses(evasión de ruido promediando)
attach(dbmeses)

# plot(mes,uni,type="l",col="red",lwd=2)
par(mfrow=c(3,2))
plot(mes,TRM,type="l")
plot(mes,uni,type="l",ylab="Unidades")
plot(mes,pop,type="l",ylab="Popularidad")
plot(mes,templeo,type="l",ylab="Tasa de empleo")
plot(mes,tdesem,type="l",ylab="Tasa de desempleo")



#IPC
ipc<-read.csv2("ipc.csv",header=T)
ipc<-ipc[-3]
ipc<-ipc[-3]
names(ipc)[3]<-"varanual"
ipc$fecha<-as.Date(paste(ipc$fecha,1,sep="-"),"%Y-%m-%d")
dbmeses<-cbind2(dbmeses,ipc$IPC)
dbmeses<-cbind2(dbmeses,ipc$varanual)
dbmeses<-cbind2(dbmeses,ipc$PromGaso)
names(dbmeses)[7]<-"IPC"
names(dbmeses)[8]<-"varanualipc"
names(dbmeses)[9]<-"PromGaso"

ipc$varanual=gsub("\\%", "", ipc$varanual)
ipc$varanual=as.numeric(gsub("\\,", ".", ipc$varanual))

vipc<-rep(ipc$IPC[1],monthDays(ipc$fecha[1]))
for (i in 2:nrow(ipc)){
  auxipc<-rep(ipc$IPC[i],monthDays(ipc$fecha[i]))
  vipc<-append(vipc,auxipc)
}
db<-cbind2(db,vipc)
names(db)[9]<-"IPC"


vvaripc<-rep(ipc$varanual[1],monthDays(ipc$fecha[1]))
for (i in 2:nrow(ipc)){
  auxvaripc<-rep(ipc$varanual[i],monthDays(ipc$fecha[i]))
  vvaripc<-append(vvaripc,auxvaripc)
}
db<-cbind2(db,vvaripc)
names(db)[10]<-"varanualipc"

vpromgaso<-rep(ipc$PromGaso[1],monthDays(ipc$fecha[1]))
for (i in 2:nrow(ipc)){
  auxvaripc<-rep(ipc$PromGaso[i],monthDays(ipc$fecha[i]))
  vpromgaso<-append(vpromgaso,auxvaripc)
}
db<-cbind2(db,vpromgaso)
names(db)[11]<-"Promgaso"

attach(db)
modelo1<-lm(Unidades~TRM+mes+dia+pop+templeo+tdesem)
