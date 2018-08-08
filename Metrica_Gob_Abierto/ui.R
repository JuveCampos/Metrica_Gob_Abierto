# Proyecto de Visualizacion
# Metrica de Gob Abierto 2016
# Jorge Juvenal Campos Ferreira

# Cambiamos el locale
#Sys.setlocale("LC_ALL", 'en_US.UTF-8')

# Paqueterias (actualizadas a junio del 2018)
library(dplyr)
library(ggplot2)
library(shiny)
library(leaflet)
library(sf)
library(shinydashboard)
library(viridis)
library(plotly)
library(png)
library(rebus)
library(stringr)

dbHeader <- dashboardHeader(title = "Metrica de Gobierno Abierto", titleWidth = 350)
#dbHeader <- dashboardHeader(title = "Metrica de Gobierno Abierto", titleWidth = 350,
#                            tags$li(a(href = 'https://www.cide.edu',
#                                      img(src = 'https://www.cide.edu/wp-content/themes/cide_general/img/logo_cide.png',
#                                          title = "CIDE", height = "30px"),
#                                      style = "padding-top:10px; padding-bottom:10px;"),
#                                    class = "dropdown"),
#                            
#                            tags$li(a(href = 'http://lnpp.cide.edu',
#                                      img(src = 'http://lnpp.cide.edu/wp-content/themes/lnpp/images/logo.svg',
#                                          title = "LNPP", height = "30px"),
#                                      style = "padding-top:10px; padding-bottom:10px;"),
#                                    class = "dropdown"),
#                            
#                            tags$li(a(href = 'http://inicio.inai.org.mx',
#                                      img(src = 'http://inicio.inai.org.mx/Style%20Library/img/logo.png',
#                                          title = "INAI", height = "30px"),
#                                      style = "padding-top:10px; padding-bottom:10px;"),
#                                    class = "dropdown")
#                            
# ) 


# Datos

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
shape <- sf::st_read(paste0(root,"Shape Estatal/sin_islas_3.shp"))
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

SO <- read.csv(paste0(root, "BSO.csv"))
#Gmaxmin <- read.csv(paste0(root, "maxmin.csv"))


#################
# S I D E B A R #
#################

sidebar <- dashboardSidebar(
  width = 350,
  
  sidebarMenu(
    
    menuItem("Nivel Estatal", tabName = "NIVEL_ESTATAL_M",
             menuSubItem("Mapa a nivel estado", tabName = "MAPA"),
             menuSubItem("Comparativa GA a Nivel Estado", tabName = "NIVEL_ESTATAL"),
             selectInput("Anio", label = "Selecciona A?o", 
                         choices = years)),
    
    menuItem("Por Sujeto Obligado", tabName = "SUJETO_OBLIGADO",
             menuSubItem("Por sujeto obligado", tabName = "SO"),
             menuSubItem("Por tipo de sujeto obligado", tabName = "TIPO_SO"),
             selectInput("Anio", label = "Selecciona A?o", 
                         choices = years)), 
    
    menuItem("Base de Datos INAI", tabName = "BASE_DATOS",
             menuSubItem("Por sujeto obligado", tabName = "BD"),
             selectInput("SO", label = "Seleccione un SO", choices = SOS, width = 400)
    )
  )
) 

###########
# B O D Y #
###########

body <- dashboardBody(
  
  tabItems(
    
    ################################################################################# BD
    tabItem("BD", 
            fluidRow(
              box(title = "Solicitudes Recibidas", status = "warning", solidHeader = TRUE, 
                  lenght = 1000, height = 550, 
                  plotlyOutput("dona_1")
              ),
              
              box(title = "Palabras mas usadas en Solicitud", status = "warning", solidHeader = TRUE, 
                  height = 550, 
                  imageOutput("plot3")
              )
            )
            
    ), #Fin de TabItem1
    ################################################################################# BD
    
    ################################################################################# SO
    tabItem("SO", 
            
            box(title = "Seleccione Estados", status = "primary", 
                width = 100,
                
                fluidRow(
                  column(3,  
                         checkboxGroupInput("g1", "", 
                                            choiceNames = g1,
                                            choiceValues = g1, 
                                            selected = g1[1])), 
                  column(3, 
                         checkboxGroupInput("g2", "",
                                            choiceNames =  g2, 
                                            choiceValues = g2)),
                  column(3, 
                         checkboxGroupInput("g3", "",
                                            choiceNames  = g3,
                                            choiceValues = g3)),
                  column(3, 
                         checkboxGroupInput("g4", "",
                                            choiceNames = g4,
                                            choiceValues = g4)) 
                )   
            ), # FIN DE LA BOX 1
            
            box("Grafica del indice GA por estado", status = "warning"
                ,width = 300, height = 600, 
                plotlyOutput("barras_edo")
            ), 
            
            box("Valores maximo, minimo y medio del Gobierno Abierto", status = "warning"
                ,width = 300, height = 500, 
                plotOutput("maxmin")
            ), 
            
            box("Componentes de la Metrica (Participacion y Transparencia)", status = "warning"
                ,width = 300, height = 500,
                plotlyOutput("comp_")
            )
            
            
    ), #Fin de TabItem1
    ################################################################################# SO
    
    
    ################################################################################# MAPA
    
    tabItem("MAPA",
            
            #fluidRow(
            
            box(
              title = "Indice de Gobierno Abierto a Nivel Entidad", status = "warning", solidHeader = TRUE, 
              height = 580, width = 380
              ,
              leafletOutput('MAPA_',height = 500),
              "Fuente: Datos Abiertos de la Metrica de Gobierno Abierto 2017 y 2019. (C) CIDE, INAI"     
            ), 
            
            box(title = "Informacion del estado", status = "primary", height = 500, width = 380, 
                plotlyOutput("Cm_1EDO"))
            #)
    )
    
    ################################################################################# MAPA
    
    
  ) # fin de tab items
  
) # final dashboard


###############
# UI y Server #
###############

ui <- dashboardPage(skin = "black", dbHeader, sidebar, body)
