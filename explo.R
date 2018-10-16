rm(list=ls())
library(dplyr)
library(MASS)
library(Hmisc)
library(xlsx)
library(lubridate)
setwd("D:/Documents/GitHub/Trabajo2")
db <- read.csv2("Autos TRM.csv",header = TRUE,stringsAsFactors = FALSE)
db$Fecha <- as.Date(db$Fecha, format = "%d/%m/%Y") #Formating the date
db$TRM=gsub("\\$", "", db$TRM)
db$TRM=gsub("\\.", "", db$TRM)
db$TRM=as.numeric(gsub("\\,", ".", db$TRM))
db$TRMdesfas=gsub("\\$", "", db$TRMdesfas)
db$TRMdesfas=gsub("\\.", "", db$TRMdesfas)
db$TRMdesfas=as.numeric(gsub("\\,", ".", db$TRMdesfas))
db<-cbind2(db,month(db$Fecha))
db<-cbind2(db,weekdays(db$Fecha))

names(db)[5]<-"mes"
names(db)[6]<-"dia"
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
names(db)[7]<-"pop"
#Invocar tasa de empleo y desempleo mensual
empleoaux<-read.csv2("Empleo_desempleo.csv",header=FALSE,stringsAsFactors = FALSE)
names(empleoaux)[1]<-"fecha"
names(empleoaux)[2]<-"templeo"
names(empleoaux)[3]<-"tdesem"
#Darle formato de día
empleoaux$fecha<-as.Date(paste(empleoaux$fecha,1,sep="-"),"%Y-%m-%d")
empleoaux<-empleoaux[order(empleoaux$fecha),]
#Incluir en el data por meses
dbmeses<-cbind2(dbmeses,empleoaux$templeo)
dbmeses<-cbind2(dbmeses,empleoaux$tdesem)
names(dbmeses)[5]<-"templeo"
names(dbmeses)[6]<-"tdesem"

#Incluir tasa de empleo por días 
vtemp<-rep(empleoaux$templeo[1],monthDays(empleoaux$fecha[1]))
for (i in 2:nrow(empleoaux)){
  auxtemp<-rep(empleoaux$templeo[i],monthDays(empleoaux$fecha[i]))
  vtemp<-append(vtemp,auxtemp)
}
db<-cbind2(db,vtemp)
names(db)[8]<-"templeo"

#Incluir tasa de desempleo
vtdesem<-rep(empleoaux$tdesem[1],monthDays(empleoaux$fecha[1]))
for (i in 2:nrow(empleoaux)){
  auxtdesem<-rep(empleoaux$tdesem[i],monthDays(empleoaux$fecha[i]))
  vtdesem<-append(vtdesem,auxtdesem)
}
db<-cbind2(db,vtdesem)
names(db)[9]<-"tdesem"

#Analizar los datos por día de la semana
semanacompra<-db%>%group_by(dia)%>%summarise(Unidades=sum(Unidades))
semanacompra$dia <- factor(semanacompra$dia, levels= c("lunes", "martes","miércoles", "jueves", "viernes", "sábado", "domingo"))
semanacompra<-semanacompra[order(semanacompra$dia), ]

#Análisis por día de la semana
# yy=barplot(semanacompra$Unidades)
# axis(1, at=yy,labels=semanacompra$dia, tick=FALSE, las=2, line=-0.5)

#Analizar los datos por mes
mescompra<-db%>%group_by(mes)%>%summarise(Unidades=sum(Unidades))

#Análisis de meses y compras
# xx=barplot(mescompra$Unidades)
# axis(1, at=xx,labels=c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"), tick=FALSE, las=2, line=-0.5, cex.axis=0.9)

#Análisis del dólar y de los carros registrados por meses(evasión de ruido promediando)




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
names(db)[10]<-"IPC"


vvaripc<-rep(ipc$varanual[1],monthDays(ipc$fecha[1]))
for (i in 2:nrow(ipc)){
  auxvaripc<-rep(ipc$varanual[i],monthDays(ipc$fecha[i]))
  vvaripc<-append(vvaripc,auxvaripc)
}
db<-cbind2(db,vvaripc)
names(db)[11]<-"varanualipc"

vpromgaso<-rep(ipc$PromGaso[1],monthDays(ipc$fecha[1]))
for (i in 2:nrow(ipc)){
  auxvaripc<-rep(ipc$PromGaso[i],monthDays(ipc$fecha[i]))
  vpromgaso<-append(vpromgaso,auxvaripc)
}
db<-cbind2(db,vpromgaso)
names(db)[12]<-"Promgaso"

#Salario mínimo Colombia
salmin<-c(566700,589000,616000,644350,689455,737717)
anosal<-seq(2012,2017)
salarmin<-data.frame(anosal,salmin)

salarmin$anosal<-paste(salarmin$anosal,1,sep="-")
salarmin$anosal<-as.Date(paste(salarmin$anosal,1,sep="-"),"%Y-%m-%d")

vsal<-rep(salarmin$salmin[1],yearDays(salarmin$anosal[1]))
for (i in 2:nrow(salarmin)){
  auxsal<-rep(salarmin$salmin[i],yearDays(salarmin$anosal[i]))
  vsal<-append(vsal,auxsal)
}
db<-cbind2(db,vsal)
names(db)[13]<-"salmini"

# diasaux<-c("lunes", "martes","miércoles", "jueves", "viernes", "sábado", "domingo")
# for (i in 1:7){
#   db$dia<-gsub(diasaux[i],i,db$dia)
# }

# vTRM<-rep(dbmeses$TRM[1],monthDays(dbmeses$mes[1]))
# for (i in 2:nrow(dbmeses)){
#   auxtrm<-rep(dbmeses$TRM[i],monthDays(dbmeses$mes[i]))
#   vTRM<-append(vTRM,auxtrm)
# }
# db$TRM<-vTRM



dtf<-read.csv2("DTF.csv",header=F)
names(dtf)[1]<-"fecha"
names(dtf)[2]<-"dtf"
dtf$fecha<-as.Date(paste(dtf$fecha,1,sep="-"),"%Y-%m-%d")
dtf<-dtf[order(dtf$fecha),]
dtf$dtf<-gsub("%","",dtf$dtf)

vdtf<-rep(dtf$dtf[1],monthDays(dtf$fecha[1]))
for (i in 2:nrow(dtf)){
  auxdtf<-rep(dtf$dtf[i],monthDays(dtf$fecha[i]))
  vdtf<-append(vdtf,auxdtf)
}
db<-cbind2(db,vdtf)
names(db)[14]<-"detf"
db$detf<-gsub(",",".",db$detf)
db$detf<-as.numeric(db$detf)

icc<-read.csv2("ICC.csv", header=T, sep=",")
icc$fecha<-as.Date(paste(icc$fecha,1,sep="-"),"%Y-%m-%d")
icc<-subset(icc, format(as.Date(icc$fecha),"%Y")!=2018)
vicc<-rep(icc$valor[1],monthDays(icc$fecha[1]))
for (i in 2:nrow(icc)){
  auxicc<-rep(icc$valor[i],monthDays(icc$fecha[i]))
  vicc<-append(vicc,auxicc)
}
db<-cbind2(db,vicc)
names(db)[15]<-"iconf"
db$iconf<-gsub(",",".",db$iconf)
db$iconf<-as.numeric(db$iconf)
db<-cbind2(db,year(db$Fecha))
names(db)[16]<-"ano"
db<-cbind2(db,day(db$Fecha))
names(db)[17]<-"dmes"

# db<-subset(db, format(as.Date(db$Fecha),"%Y")!=2017)
# dbmeses<-subset(dbmeses, format(as.Date(dbmeses$mes),"%Y")!=2017)
db2016<-subset(db, format(as.Date(db$Fecha),"%Y")!=2017)
dbmeses2016<-subset(dbmeses, format(as.Date(dbmeses$mes),"%Y")!=2017)
db2017<-subset(db, format(as.Date(db$Fecha),"%Y")==2017)
dbmeses2017<-subset(dbmeses, format(as.Date(dbmeses$mes),"%Y")==2017)
attach(db2016)
modelo1<-lm(Unidades~mes+dia+pop+templeo+tdesem+IPC+varanualipc+Promgaso+salmini+detf+iconf)
modelosel<-stepAIC(object=modelo1, trace=FALSE, direction="backward", k=2)
#
modposin<-glm(Unidades~mes+dia+templeo+tdesem+IPC+Promgaso+salmini+detf+iconf+dmes,poisson)
#modelosel<-stepAIC(object=mod_pois_glm, trace=FALSE, direction="backward", k=2)

mod_pois_glm<-glm(Unidades~mes+dia+ano+dmes,poisson)
1-(mod_pois_glm$deviance/mod_pois_glm$null.deviance)
1-(modposin$deviance/modposin$null.deviance)
# 1-(modelosel$deviance/modelosel$null.deviance)
# prueba<-db2017[6,]
# round(exp(predict.glm(mod_pois_glm,newdata=prueba)))
# ccfvalues<-ccf(db2016$TRM,db2016$Unidades,200)
# min(ccfvalues)

#predict.glm(mod_pois_glm,newdata=db2017)
estim<-exp(predict.glm(mod_pois_glm,newdata=db2017))
estim<-as.data.frame(estim)
names(estim)[1]<-"fit"

# estim$fit<-round(estim$fit)

#R2
r2<-1-(sum((db2017$Unidades-estim$fit)^2)/sum((db2017$Unidades-mean(db2017$Unidades))^2))
attach(dbmeses)

#Función descomposición Shiny
daygen<-function(fi,ff){
  vecfec<-as.data.frame(seq(as.Date(fi),as.Date(ff),by="day"))
  names(vecfec)[1]<-"Fecha"
  vecfec$dia<-weekdays(vecfec$Fecha)
  vecfec$mes<-month(vecfec$Fecha)
  vecfec$ano<-year(vecfec$Fecha)
  vecfec$dmes<-day(vecfec$Fecha)
  auxfun<-round(exp(predict.glm(mod_pois_glm,newdata=vecfec)))
  final<-as.data.frame(vecfec$Fecha)
  final<-cbind2(final,auxfun)
  names(final)[2]<-"Estimado"
  return(final)
}



predic2018<-read.csv2("fecha.csv",header=F)
names(predic2018)[1]<-"Fecha"
predic2018$Fecha<- as.Date(predic2018$Fecha, format = "%d/%m/%Y")
predic2018$mes<-month(predic2018$Fecha)
predic2018$dia<-weekdays(predic2018$Fecha)
predic2018$ano<-year(predic2018$Fecha)
predic2018$dmes<-day(predic2018$Fecha)
aux2018<-exp(predict.glm(mod_pois_glm,newdata=predic2018))
pred2018<-predic2018$Fecha
pred2018<-as.data.frame(predic2018$Fecha)
pred2018<-cbind2(pred2018,aux2018)
names(pred2018)[2]<-"Estimado"
names(pred2018)[1]<-"Fecha"


# plot(mes,uni,type="l",col="red",lwd=2)
par(mfrow=c(2,2))
plot(mes,TRM,type="l")
plot(mes,uni,type="l",ylab="Unidades",col="red",lwd=2)
plot(mes,pop,type="l",ylab="Popularidad")
plot(mes,templeo,type="l",ylab="Tasa de empleo")
plot(mes,tdesem,type="l",ylab="Tasa de desempleo")
plot(mes,IPC,type="l",ylab="IPC")
plot(mes,varanualipc,type="l",ylab="Variación anual IPC")
plot(mes,varanualipc,type="l",ylab="Costo promedio en dólares barril de petróleo NY")

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
axis(1, at=xx,labels=c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"), tick=FALSE, las=2, line=-0.5, cex.axis=0.9)

