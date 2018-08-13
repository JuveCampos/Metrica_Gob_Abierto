# Proyecto de Visualizacion
# Metrica de Gob Abierto 2016
# Jorge Juvenal Campos Ferreira

shinyUI({

  dbHeader <- dashboardHeader(title = "Metrica de Gobierno Abierto 2019", titleWidth = 350,
                              tags$li(a(href = 'https://www.cide.edu',
                                        img(src = 'https://www.cide.edu/wp-content/themes/cide_general/img/logo_cide.png',
                                        title = "CIDE", height = "30px"),
                                        style = "padding-top:10px; padding-bottom:10px;"),
                                      class = "dropdown"),
                              tags$li(a(href = 'http://lnpp.cide.edu',
                                        img(src = 'http://lnpp.cide.edu/wp-content/themes/lnpp/images/logo.svg',
                                        title = "LNPP", height = "30px"),
                                        style = "padding-top:10px; padding-bottom:10px;"),
                                      class = "dropdown"),
                              tags$li(a(href = 'http://inicio.inai.org.mx',
                                        img(src = 'http://inicio.inai.org.mx/Style%20Library/img/logo.png',
                                        title = "INAI", height = "30px"),
                                        style = "padding-top:10px; padding-bottom:10px;"),
                                      class = "dropdown"))

#################
# S I D E B A R #
#################

sidebar <- dashboardSidebar(
  width = 350,
  
  sidebarMenu(
    
    menuItem("Introduccion", tabName = "INTRO"),
    
    
    menuItem("Base de Datos", tabName = 'tabBASE'),
    
    
    menuItem("Nivel Estatal", tabName = "NIVEL_ESTATAL_M",
             menuSubItem("Mapa a nivel estado", tabName = "MAPA"),
             menuSubItem("Comparativa GA a Nivel Estado", tabName = "NIVEL_ESTATAL"),
             selectInput("Anio", label = "Selecciona Año", 
                         choices = years), br()),
    
  # menuItem("Por Sujeto Obligado", tabName = "SUJETO_OBLIGADO",
  #           menuSubItem("Comparacion entre Sujetos Obligados", tabName = "SO"),
  #           ## SELECT INPUTS DE LA VENTANA SELECCIONADA ## 
  #           selectInput("Anio", label = "Selecciona Año", 
  #                       choices = years),
  #           selectInput("SELECTedo", label = "Seleccione estado",   
  #                       choices = as.list(prom$estado)),
  #           selectInput("SELECTtipoSO", label = "Seleccione tipo de Sujeto Obligado",  
  #                       choices = as.list(levels(SO$ts))),
  #           selectInput("SELECTso", label = "Seleccione el Sujeto Obligado",  
  #                       choices = SO$SO,
  #                       multiple = TRUE
  #                       ),
  #           actionButton(inputId = "btnIncluir", label = "Incluir SO")
           
   #          ),  # Fin del SideBar Menu 
    
    menuItem("Base de Datos INAI", tabName = "BaseDatos",
             menuSubItem("Por sujeto obligado", tabName = "BD"),
             selectInput("LEVEL", label = "Seleccione un estado", choices = as.list(prom$estado), selected = "Federal"),
             selectInput("SO", label = "Seleccione un SO", choices = SOS, width = 400),
             br()
    )
  )
) 

###########
# B O D Y #
###########

body <- dashboardBody( 
  
  tags$head(tags$style(
    HTML('
                .content-wrapper {
                      background-color: linen !important;
                }

         body, label, input, button, select { 
         font-family: "Arial";
         font-color = "yellow";
         }')
    )),
  
  tabItems(
    
    ################################################################################# INTRO
    tabItem("INTRO"
            , includeMarkdown("Texto Introduccion.Rmd"),
            
    htmlOutput("pdfview")),
    
    ################################################################################# INTRO
    
    ################################################################################# COMPARATIVA ENTRE SOS
    tabItem("SO",  
            fluidPage(
              fluidRow(
                column(12, box(title = "Sujetos Obligados Seleccionados: ", status = "primary", textOutput("SOselect")))
              ) #Fin del FluidRow
            ) # Fin del fluidpage
          ), # Fin de TabItem
    
    ################################################################################# COMPARATIVA ENTRE SOS
    
    ################################################################################# Mostrar BD completa
    tabItem("tabBASE",
      fluidPage(h1("Base De Datos - Sujetos Obligados de la Metrica de Gobierno Abierto"),
                tags$style("h1 {
                           color: rgb(46,123,135);
                                    }
                           "),
                
          fluidRow(column(12, box(title = "Datos", width = 550, status = "primary", DT::dataTableOutput("BASE_DE_DATOS")))
          ), 
                downloadButton("download_data", label = "Descargar")
      ) #Fin del fluidPage
    ), #Fin del TabItem
    
    ################################################################################# Mostrar BD completa
    
    
    
    ################################################################################# BD
    tabItem("BD", 
          fluidPage(
            fluidRow(
              column(6,box(title = "Respuesta a Solicitudes de Informacion", status = "primary", width = 550, plotlyOutput("dona_1"))),
              column(6, fluidRow(column(12, box(title = "Palabras mas utilizadas en Solicitud", width = 550, status = "primary")),
                       column(12, imageOutput("plot3"))))
            ), br(), br(), br()
            #,fluidRow(
            #  column(12, box(title = "Tiempo de respuesta a Solicitud de informacion (dias)", 
            #                 status = "warning", width = 1000,
            #                 shinycssloaders::withSpinner(plotlyOutput("gphTiempoRespuesta"))))
            #)
          )
    
    ), #Fin de TabItem1
    ################################################################################# BD
    
    ################################################################################# SO
    tabItem("NIVEL_ESTATAL", 
            
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
                                            choiceValues = g4), 
                         actionButton(inputId = "btnTodos",   label = "Seleccionar Todos"),
                         br(), br(),
                         actionButton(inputId = "btnNinguno", label = "Deseleccionar")
                         ) 
                )   
            ), # FIN DE LA BOX 1
            
            box("Grafica del indice GA por estado", status = "warning"
                ,width = 300, height = 600, 
                shinycssloaders::withSpinner(plotlyOutput("barras_edo"))
            ), 
            
            box("Valores maximo, minimo y medio del Gobierno Abierto", status = "warning"
                ,width = 300, height = 500, 
                plotlyOutput("maxmin")
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
              shinycssloaders::withSpinner(leafletOutput('MAPA_',height = 500)),
              "Fuente: Datos Abiertos de la Metrica de Gobierno Abierto 2017 y 2019. (C) CIDE, INAI"     
            ), 
            
            #tabBox(
            #  title = "Informacion de estado seleccionado",
            #  # The id lets us use input$tabset1 on the server to find the current tab
            #  id = "tabset1", height = 580,
            #  tabPanel("Componentes de la metrica", shinycssloaders::withSpinner(plotlyOutput("Cm_1EDO"))),
            #  tabPanel("Por tipo de Sujeto Obligado", shinycssloaders::withSpinner(plotlyOutput("plotTIPOSO")))
            #)
            
            fluidPage(
              fluidRow(
                column(12, box(title = "Dimensiones de la metrica de Gobierno Abierto para el estado Seleccionado", 
                               status = "success", solidHeader = TRUE, height = 500, width = 380, shinycssloaders::withSpinner(plotlyOutput("Cm_1EDO")),
                               "Fuente: Datos Abiertos de la Metrica de Gobierno Abierto 2017 y 2019. (C) CIDE, INAI"))),
                fluidRow(column(12, box(title = "Indice de la metrica de Gobierno Abierto por Tipo de Sujeto Obligado", 
                                      status = "info", solidHeader = TRUE, height = 500, width = 380, shinycssloaders::withSpinner(plotlyOutput("plotTIPOSO")),
                                      "Fuente: Datos Abiertos de la Metrica de Gobierno Abierto 2017 y 2019. (C) CIDE, INAI"                        )))
               )
            
            #)
            
            #box(title= "Informacion de estado seleccionado", status = "primary", height = 500, width = 380, 
            #    shinycssloaders::withSpinner(plotlyOutput("Cm_1EDO")))
            #)
    )
    
    ################################################################################# MAPA
    
  ) # fin de tab items
  
) # final dashboard

dashboardPage(skin = "black", dbHeader, sidebar, body)

})