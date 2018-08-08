
server <- function(input, output) {
  
  ###################################################### pol_of_click event
  
  pol_of_click <- reactiveValues(clickedShape = NULL)
  
  observeEvent(input$MAPA__shape_click, 
               {
                 pol_of_click <- input$pol_of_click
                 p <- input$MAPA__shape_click$id
                 #print(p)
                 pol_of_click$clickedShape <- input$MAPA__shape_click$id
                 print(as.numeric(pol_of_click$clickedShape))
               })
  
  ###################################################### pol_of_click event
  
  ################################################################################# Cm_1EDO
  output$Cm_1EDO <- renderPlotly({
    
    i <- as.numeric(input$MAPA__shape_click$id) 
    
    dt <- prom[c(1:10, 12:33),]
    dt$num <- c(1:32)
    dato <- subset.data.frame(dt, dt$num == i)
    
    plot_ly(dato, x = dato$estado, y = dato$GA, type = 'bar', name = 'Gobierno Abierto')%>%
      add_trace(y = dato$P, name = 'Participacion', marker = list(color = 'rgb(110,44,18)'))%>%
      add_trace(y = dato$PG, name = 'Participacion desde el Gobierno', marker = list(color = 'rgb(183,70,29)')) %>%
      add_trace(y = dato$PC, name = 'Participacion desde el Ciudadano', marker = list(color = 'rgb(239,101,113)')) %>%
      add_trace(y = dato$T, name = 'Transparencia', marker = list(color = 'rgb(42,70,26)')) %>%
      add_trace(y = dato$TG, name = 'Transparencia desde Gobierno', marker = list(color = 'rgb(67,113,40)')) %>%
      add_trace(y = dato$TC, name = 'Transparencia desde Ciudadano', marker = list(color = 'rgb(133,200,123)')) %>%
      layout(yaxis = list(title = 'Valor Indice'), barmode = 'group', bargap=0.15, bargroupgap = 0.1)
    
  })
  
  ################################################################################# Cm_1EDO
  
  
  ###################################################### DONA_1
  output$dona_1 <- renderPlotly({
    
    z <- which(DR$SOS == input$SO)
    c <- 2:20
    
    plot_ly(DR, 
            labels = TR, 
            values = as.character(DR[z,c])
            ,insidetextfont=list(color = "#FFFFFF")
            #,
            # marker = list(colors =  #topo.colors(20), 
            #              factpal(TR),
            #              line = list(color = '#FFFFFF', width = 1))
            ,showlegend = T) %>%
      
      add_pie(hole = 0.6) %>%
      layout(title = DR$SO[z],  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      config(displayModeBar = F)
  })
  ###################################################### DONA_1
  
  ###################################################### MAPA_
  output$MAPA_ <- renderLeaflet({
    # Paleta #
    #pal <- colorNumeric(
    #  palette = "YlGnBu",
    #  domain = shape$GA
    #)
    
    ## custom label format function
    myLabelFormat = function(..., reverse_order = FALSE){ 
      if(reverse_order){ 
        function(type = "numeric", cuts){ 
          cuts <- sort(cuts, decreasing = T)
        } 
      }else{
        labelFormat(...)
      }
    }
    
    pal <- colorNumeric(rev("magma"),domain = shape$GA)
    specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
    mnpopup <- paste0("<b>","Nombre estado: ","</b>",               shape$estado, "<br>",
                      "<b>", "Indice de Gobierno Abierto: ",    "</b>", specify_decimal(shape$GA, 3) , "<br>",
                      "<b>", "Puntaje de Participacion Ciudadana: ",     "</b>", specify_decimal(shape$P,3) , "<br>", 
                      "<b>", "Puntaje de Transparencia: ",     "</b>", specify_decimal(shape$T,3) , "<br>" )
    
    leaflet(shape) %>%
      addProviderTiles("CartoDB.Positron") %>%
      # addPolygons()
      addPolygons(color = "#444444" ,
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 1.0,
                  fillOpacity = 0.5,
                  fillColor = ~pal(shape$GA),
                  layerId = ~shape$CVE_EDO,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label = ~shape$estado,
                  labelOptions = labelOptions(direction = "auto"),
                  popup = mnpopup
      ) %>%
      addLegend(position = "topright", pal = pal, values = ~shape$GA,
                title = "Indice de Gobierno Abierto", labFormat = myLabelFormat(reverse_order = F))
  })
  
  ###################################################### MAPA_
  
  
  ###################################################### barras_edo
  output$barras_edo <- renderPlotly({
    seleccion <- c(input$g1, input$g2, input$g3, input$g4)
    x = seleccion
    y = as.character(prom$GA[str_detect(prom$estado, pattern = or1(exactly(seleccion)))])
    
    plot_ly(y=y, x=x, histfunc='sum', type = "histogram") %>%
      layout(yaxis=list(type='linear'))  %>%
      layout(autosize = T) 
  })
  ###################################################### barras_edo
  
  ###################################################### max_min_edo
  output$maxmin <- renderPlot({
    seleccion <- c(input$g1, input$g2, input$g3, input$g4)
    
    SO_ <- SO
    
    SO_$dummy <- str_detect(SO_$edo, pattern = or1(exactly(seleccion)))
    
    SO_ <- SO_ %>%
      filter(dummy == TRUE)
    
    m_1 <- SO_ %>% 
      group_by(edo) %>%
      summarize(min_GA = min(GA), prom_GA = mean(GA), max_GA = max(GA)) %>%
      arrange(min_GA, prom_GA, max_GA)
    
    ggplot(SO_, aes(x = edo, y = GA)) +
      geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4) +
      geom_point(data = m_1, aes(x = edo, y = min_GA), colour = "blue", size = 5) +
      geom_point(data = m_1, aes(x = edo, y = prom_GA), colour = "green", size = 5) +
      geom_point(data = m_1, aes(x = edo, y = max_GA), colour = "orange", size = 5) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  ###################################################### max_min_edo
  
  ###################################################### comp_
  output$comp_ <- renderPlotly({
    seleccion <- c(input$g1, input$g2, input$g3, input$g4)
    data <- prom
    plot_ly(data, x =prom$estado[str_detect(prom$estado, pattern = or1(exactly(seleccion)))] , 
            y = prom$P[str_detect(prom$estado, pattern = or1(exactly(seleccion)))], type = 'bar', name = 'Participacion') %>%
      add_trace(y = ~prom$T[str_detect(prom$estado, pattern = or1(exactly(seleccion)))], name = 'Transparencia') %>%
      layout(yaxis = list(title = 'Valor Indice'), barmode = 'group')
  })
  ###################################################### comp_
  
  
  ###################################################### NUBE_1
  output$plot3 <- renderImage({
    
    # Indice del selector
    z <- which(DR$SOS == input$SO)
    
    #filename <- normalizePath(file.path('/Users/admin/Desktop/Proyectos/CIDE/Seminarios_CIDE/Datos Grande/clave',
    #                                    paste('image', input$n, '.jpeg', sep='')))
    
    filename <- paste0(root, 'Datos Grande/clave', as.character(claves[z]), '.png')
    
    # Return a list containing the filename
    list(src = filename,
         contentType = "image/png", 
         alt = "WORDCLOUD")
  }, deleteFile = FALSE)
  
  ###################################################### NUBE_1
  
} # final de server


#########
# A P P #
#########

shinyApp(ui, server)