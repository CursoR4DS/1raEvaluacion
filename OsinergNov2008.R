rm(list = ls())
setwd(dir="C:/Users/user/Desktop/R_CTIC/1raEvaluacion/2008")
getwd()
dir()

OsinergNov2018 <- read.table("201811_TABLA04_SICLI.txt", header = TRUE,sep = "\t",
                               col.names = c("CodEmpresa","Suministro", "PuntoSuministro","Fecha","RegistroActiva","RegistroPasiva","Periodo"),
                               colClasses = c("factor","factor","factor","character","numeric", "numeric", "character"))

class(OsinergNov2018)
str(OsinergNov2018)
dim(OsinergNov2018)

#### Conversion de la variable fecha ####
OsinergNov2018$Fecha

View(OsinergNov2018)

library(lubridate)
library(help="lubridate")

# Creamos la variable FechaDate para recuperar la cadena que se tiene en el 
# data frame original 
OsinergNov2018$FechaDate <- ymd_hm(OsinergNov2018$Fecha)

# Usando funciones de lubridate

OsinergNov2018$year <- year(OsinergNov2018$FechaDate)
OsinergNov2018$month <- month(OsinergNov2018$FechaDate)
OsinergNov2018$day <- day(OsinergNov2018$FechaDate)
OsinergNov2018$hour <- hour(OsinergNov2018$FechaDate)
OsinergNov2018$minute <- minute(OsinergNov2018$FechaDate)

#Convirtiendo a formato Fecha (Year-Month-Day)
OsinergNov2018$FechaYMD <- as.date(OsinergNov2018$FechaDate)

View(OsinergNov2018)

library(dplyr)

# Utilicemos una estructura FOR para crear un Data Frame donde
# almacenaremos un resumen por DIA
Resumen <-data.frame(Dia=integer(),
                     RegistroActivaTotal=double())

#Data Frame temportal
NuevaFila<- data.frame()

for (y in unique(OsinergNov2018$day)){
        NuevaFila<- data.frame(Dia=y, 
                               RegistroActivaTotal=sum(OsinergNov2018[OsinergNov2018$day==y, ]$RegistroActiva))
        Resumen<-rbind(Resumen,NuevaFila)
}

View(Resumen)

#### Graficos de dispersion ####

head(OsinergNov2018, n = 1)$FechaYMD # primer dia de la data 
tail(OsinergNov2018, n = 1)$FechaYMD # ultimo dia de la data 

# Empecemos usando la libreria graphics

x11()
jpeg("OsinergNov2018_1.jpeg") 
plot(x = Resumen$Dia, y= Resumen$RegistroActivaTotal,type ="l",
     xlab = paste("Fecha [" , head(OsinergNov2018, n = 1)$FechaYMD , "-", tail(OsinergNov2018, n = 1)$FechaYMD,"]"),
     ylab = "Registro de Energía Activa Total [kW.h]",
     main = "Osinergmin Data [Noviembre 2018]",
     sub = "Fuente : https://www.osinergmin.gob.pe/seccion/institucional/regulacion-tarifaria/publicaciones/regulacion-tarifaria")
     dev.off()

     
#### grafico de dispersion en ggplot2 ####
library(ggplot2)
     
x11()
jpeg("OsinergNov2018_2.jpeg")  
ggplot(data = Resumen, mapping = aes(x = Dia, y = RegistroActivaTotal))+
       geom_line()+
       xlab(paste("Fecha [" , head(OsinergNov2018, n = 1)$FechaYMD , "-", tail(OsinergNov2018, n = 1)$FechaYMD,"]"))+
       ylab("Registro de Energía Activa Total [kW.h]")+
       ggtitle("Osinergmin Data [Noviembre 2018]")+
       labs(subtitle = "Fuente : https://www.osinergmin.gob.pe/seccion/institucional/regulacion-tarifaria/publicaciones/regulacion-tarifaria",
        caption = "Evaluacion 1:CTIC-UNI")
dev.off()


library(hrbrthemes)
library(viridis)


View(OsinergNov2018)
View(Resumen)

# uso del operador %>% (pipe/tuberia)

x11()
jpeg("OsinergNov2018_03.jpeg")  
Resumen %>%
        ggplot(aes(x=Dia, y = RegistroActivaTotal, colour=Dia))+
        geom_line() + #capa geometrica
        # escala de color para variables continuas
        scale_colour_viridis()+
        theme_ipsum()+
        xlab(paste("Fecha [" , head(OsinergNov2018, n = 1)$FechaYMD , "-", tail(OsinergNov2018, n = 1)$FechaYMD,"]"))+
        ylab("Registro de Energía Activa Total [kW.h]")+
        ggtitle("Osinergmin Data [Noviembre 2018]")+
        labs(subtitle = "Fuente : https://www.osinergmin.gob.pe/seccion/institucional/regulacion-tarifaria/publicaciones/regulacion-tarifaria",
             caption = "Evaluacion 1:CTIC-UNI")+
        theme( #https://github.com/CursoR4DS/1raEvaluacion
                legend.position = c(0.10, 0.85),
                plot.title = element_text(size=18,hjust = 1),
                plot.subtitle = element_text(size=9,hjust = 1)
                
        )
dev.off()



# No Convertir la variable hour en variable categorica (factor)
# para poder graficar con los valores de la hora de manera DISCRETA

str(OsinergNov2018)
library(dplyr)

#OsinergNov2018 <- OsinergNov2018 %>%
#        mutate(hour= as.factor(hour))

str(OsinergNov2018)

#Creamos un data frame para los promedios por hora
PromedioHora <- OsinergNov2018 %>%
        group_by(hour) %>%
        summarise(Numero = n(), Promedio=mean(RegistroActiva, na.rm=TRUE))

help("group_by")
View(PromedioHora)

str(PromedioHora)

#Mostramos el comportamiento promedio por hora

#PromedioHora <- PromedioHora %>%
#        mutate(hour= 0:23)

x11()

jpeg("OsinergNov2018_4.jpeg")  
PromedioHora %>%
        #Para que Hour sea discreto
        #mutate(hour=as.factor(hour))%>%
        ggplot(mapping = aes(x=hour, y=Promedio, colour=hour))+
        geom_point()+
        geom_line()+
        labs(title = "Promedio por Hora del RegistroActiva",
             subtitle  = "Fuente : https://www.osinergmin.gob.pe/seccion/institucional/regulacion-tarifaria/publicaciones/regulacion-tarifaria",
             caption = "Evaluacion 1:CTIC-UNI")+
        ylab("Registro de Energía Activa Total [kW.h]")+
        xlab(paste("Rango Horario :", min(PromedioHora$hour), "-", max(PromedioHora$hour)))
dev.off()



#### calculos y graficos de resumen ####

# Se puede observar que el mayor promedio se encuentra desde las 10:00 hasta 15:00 horas
# Vamos a comparar los promedios de las empresas ELP, SHOU, CEEP, EMGH, OREP
View(OsinergNov2018)
str(OsinergNov2018)

# Hacemos filtros para rangos de horario
hour15 <- OsinergNov2018 %>%filter(hour==15) #filter usando pipe
hour1015 <- filter(.data=OsinergNov2018,hour>10 & OsinergNov2018$hour<=15 & (CodEmpresa=="ELP" | CodEmpresa=="SHOU" | CodEmpresa=="CEEP" | CodEmpresa=="EMGH" | CodEmpresa=="OREP") )
hour0023 <- filter(.data=OsinergNov2018,hour>=0 & OsinergNov2018$hour<=23 & (CodEmpresa=="ELP" | CodEmpresa=="SHOU" | CodEmpresa=="CEEP" | CodEmpresa=="EMGH" | CodEmpresa=="OREP") )

View(hour1015)
View(hour0023)

#Creamos un data frame para los promedios de las empresas por horas 0-23
PromedioEmpresa <- hour0023 %>%
        group_by(hour, CodEmpresa) %>%
        summarise(Numero = n(), Promedio=mean(RegistroActiva, na.rm=TRUE))

View(PromedioEmpresa)   


x11()

jpeg("OsinergNov2018_5.jpeg")  
PromedioEmpresa %>%
        ggplot(mapping = aes(x=hour, y=Promedio, colour=CodEmpresa))+
        geom_point()+
        geom_line()+
        labs(title = "Promedio de Empresas Hora",
             subtitle  = "Fuente : https://www.osinergmin.gob.pe/seccion/institucional/regulacion-tarifaria/publicaciones/regulacion-tarifaria",
             caption = "Evaluacion 1:CTIC-UNI")+
        ylab("Promedio de Energía Activa [kW.h]")+
        scale_x_continuous(breaks = c(0:23))+
        xlab(paste("Rango Horario :", min(PromedioEmpresa$hour), "hrs - ", max(PromedioEmpresa$hour) , "hrs"))
dev.off()
  
ggsave("OsinergNov2018_5.jpeg",width=9, height=9, dpi=72)