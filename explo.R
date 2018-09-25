db <- read.csv2("C:/Users/user/Desktop/Universdad/TAE/trabajo02/registros_autos_entrenamiento.csv",header = TRUE)
db$Fecha <- as.Date(db$Fecha, format = "%d/%m/%Y") #Formating the date

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