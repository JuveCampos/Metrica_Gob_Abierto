# Cambiamos el locale
# Sys.setlocale("LC_ALL", 'LANG=en_US.UTF-8') 
# Sys.setlocale("LC_TIME", "English")
# Sys.setlocale("LC_ALL", "UTF-8")
#options(encoding = 'UTF-8')

# Paqueterias (actualizadas a junio del 2018)

# install.packages("sf")

library(dplyr)
library(ggplot2)
library(ggthemes)
library(shiny)
library(leaflet)
library(sf)
library(shinydashboard)
library(viridis)
library(plotly)
library(png)
#library(rebus)
library(stringr)


#root <- "C:/Users/EQUIPO 9/Desktop/USB
root <- ""
prom <- read.csv("prom.csv")
DR <- read.csv(paste0(root, "DR.csv"), encoding = "UTF-8")
TR <- c("Entrega de informacion en medio electronico"                                     
        , "Inexistencia de la informacion solicitada"                                       
        , "Informacion parcialmente reservada o confidencial"                               
        , "La informacion esta disponible publicamente"                                     
        , "La solicitud no corresponde al marco de la Ley "                                 
        , "Negativa por ser reservada o confidencial"                                       
        , "No es de competencia de la unidad de enlace"                                     
        , "No se dara tramite a la solicitud"                                               
        , "Notificacion de cambio de tipo de solicitud"                                     
        , "Notificacion de disponibilidad de informacion"                                   
        , "Notificacion de envio"                                                           
        , "Notificacion de pago"                                                            
        , "Notificacion de prorroga"                                                        
        , "Notificacion lugar y fecha de entrega"                                           
        , "Requerimiento de informacion adicional"                                          
        , "Respuesta a solicitud de informacion adicional"                                  
        , "Respuesta del solicitante a la notificacion de entrega de informacion con  costo"
        , "Respuesta del solicitante a la notificacion de entrega de informacion sin costo" 
        , "Sin respuesta")  

claves <-    c(1100,61100,1200,10111,35100,3200,10102,20285,11249,20312,9121,11318,16111,40100,20410,6738,22100,11323,22300,22310,22320,
               2100,22330,22340,22350,22360,22370,22380,18572,17,8,20,10,11,4,6,27,5,12,21,22103,1300,60104,60122,3300,3100,32100,29004,64300 ,64400)

for (i in 1:50){
  for (j in 1:21){
    if(DR[i,j] == 0){
      DR[i,j] <- NA
    }
  }
}

# Shapefile
shape <- sf::st_read(paste0(root,"sin_islas_3.shp"))
names(shape)[5] <- "estado"
shape <- merge(shape, prom, by= "estado")
shape$CVE_EDO <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
                   "25", "26", "27", "28", "29", "30", "31", "32")

## GRUPOS ## 
g1 <- as.list(c("Aguascalientes","Baja California", "Baja California Sur","Campeche", "Chiapas", "Chihuahua" ,
                "Ciudad de Mexico", "Coahuila", "Colima"))
g2 <- as.list(c("Durango", "Federal", "Guanajuato","Guerrero", "Hidalgo","Jalisco","Mexico","Michoacan","Morelos"))
g3 <- as.list(c("Nayarit","NuevoLeon","Oaxaca","Puebla","Queretaro","Quintana Roo","San Luis Potosi","Sinaloa","Sonora"))
g4 <- as.list(c("Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatan","Zacatecas"))

# Listas
years <- list("2016", "2018")
SOS <- as.list(DR$SOS)

# DATOS POR INCORPORAR

SO <- read.csv(paste0(root, "BSO.csv")
               #, encoding = "UTF-8"
               , stringsAsFactors = F)
               
#Gmaxmin <- read.csv(paste0(root, "maxmin.csv"))
# object.size(SO)

inai <- read.csv("inai.csv")
tiempo_prom <- inai %>%
  group_by(Sujeto.Obligado) %>%
  summarize(promedio = mean(diferencia))

dt <- prom[c(1:10, 12:33),]
dt$num <- c(1:32)

