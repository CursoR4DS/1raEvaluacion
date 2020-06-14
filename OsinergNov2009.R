rm(list = ls())
setwd(dir="C:/Users/user/Desktop/R_CTIC/1raEvaluacion/2009")
getwd()
dir()

#Cargar los datos con decimales como caracter
# OsinergNov2019 <- read.table("201911_TABLA4.txt", header = TRUE,sep = "\t",
#                              col.names = c("CodEmpresa","Suministro", "PuntoSuministro","Fecha","RegistroActiva","RegistroPasiva","Periodo"),
#                              colClasses = c("factor","factor","factor","character","character", "character", "character"))

#A traves de IMPORT DATASET, se puede personalizar la carga de datos
OsinergNov2019 <- read.delim2("C:/Users/user/Desktop/R_CTIC/1raEvaluacion/2009/201911_TABLA4.txt")

View(OsinergNov2019)
str(OsinergNov2019)

colnames(OsinergNov2019)[1]<-"CodEmpresa"
colnames(OsinergNov2019)[2]<-"Suministro"
colnames(OsinergNov2019)[3]<-"PuntoSuministro"
colnames(OsinergNov2019)[4]<-"Fecha"
colnames(OsinergNov2019)[5]<-"RegistroActiva"
colnames(OsinergNov2019)[6]<-"RegistroPasiva"
colnames(OsinergNov2019)[7]<-"Periodo"

library(dplyr)
library(lubridate)
help(lubridate)

# debemos contar la cantidad de caracteres que forman la cadena de caracteres FECHA
nchar(OsinergNov2019$Fecha[1])

# definir nuestras variables de interes
OsinergNov2019$DayMonthYear <- substr(x = OsinergNov2019$Fecha,start = 1,stop = 8)
OsinergNov2019$hora<- substr(x = OsinergNov2019$Fecha,start = 10,stop = 11)
OsinergNov2019$minuto <- substr(x = OsinergNov2019$Fecha,start = 13,stop = 14)
OsinergNov2019$Meridiano <- substr(x = OsinergNov2019$Fecha,start = 29,stop = 30)

# Ten cuenta lo siguiente para crear una variable hora en formato de 24 horas (Hora24)
OsinergNov2019$Hora24 <- ifelse(OsinergNov2019$Meridiano=="AM", as.integer(OsinergNov2019$hora) ,as.integer(OsinergNov2019$hora)+12  )

# Vamos a cambiar el formato a OsinergNov2019$DayMonthYear en Date, y la hora, minuto en INT
OsinergNov2019$DayMonthYear <- dmy(OsinergNov2019$DayMonthYear)

OsinergNov2019$year <- year(OsinergNov2019$DayMonthYear)
OsinergNov2019$month <- month(OsinergNov2019$DayMonthYear)
OsinergNov2019$day <- day(OsinergNov2019$DayMonthYear)

View(OsinergNov2019)
str(OsinergNov2019)

library(dplyr)

#Creamos un data frame para los promedios por hora

PromedioHora2019 <- OsinergNov2019 %>%
  group_by(Hora24) %>%
  summarise(Numero = n(), Promedio2019=mean(RegistroActiva, na.rm=TRUE))

colnames(PromedioHora2019)[1]<-"Hora"
colnames(PromedioHora2019)[3]<-"Promedio"
View(PromedioHora2019)


x11()
#Almacenamos en una variable Grafico
Grafico <- PromedioHora2019 %>%
  ggplot(mapping = aes(x=Hora, y=Promedio, colour=Hora))+
  geom_point()+
  geom_line()+
  labs(title = "Linea de Tendencia del Promedio del RegistroActiva por Hora",
       subtitle  = "Fuente : https://www.osinergmin.gob.pe/seccion/institucional/regulacion-tarifaria/publicaciones/regulacion-tarifaria",
       caption = "Evaluacion 1:CTIC-UNI")+
  ylab("Promedio Registro de Energía [kW.h]")+
  xlab(paste("Rango Horario :", min(PromedioHora2019$Hora), " hrs -", max(PromedioHora2019$Hora) ," hrs"))


#Utilizando geom smooth
x11()
jpeg("OsinergNov2019_6.jpeg") 
Grafico + geom_smooth(method = "lm", formula = y ~poly(x,3))+
theme(legend.background=element_rect(fill="#A2AFC9"))
dev.off()








