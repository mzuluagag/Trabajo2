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


attach(dbmeses)
plot(mes,TRM,type="l",lwd=2)
plot(mes,uni,type="l",col="red",lwd=2)

sep2012 <- db[which(format(db$Fecha, "%Y") == 2012),]
sep2013 <- db[which(format(db$Fecha, "%Y") == 2013),]
sep2014 <- db[which(format(db$Fecha, "%Y") == 2014),]
sep2015 <- db[which(format(db$Fecha, "%Y") == 2015),]
sep2016 <- db[which(format(db$Fecha, "%Y") == 2016),]
sep2017 <- db[which(format(db$Fecha, "%Y") == 2017),]

s2 <- sum(sep2012$Unidades)
s3 <- sum(sep2013$Unidades)
s4 <- sum(sep2014$Unidades)
s5 <- sum(sep2015$Unidades)
s6 <- sum(sep2016$Unidades)
s7 <- sum(sep2017$Unidades)

barplot(height = c(s2,s3,s4,s5,s6,s7),names.arg = c("2012","2013","2014","2015","2016","2017"),
        main = "Número de carros registrados",xlab = "Año",ylab="Cantidad")
s2
s3
s4
s5
s6
s7