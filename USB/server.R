shinyServer(function(input, output, session) {
  
  ###################################################### BASE_DE_DATOS
  
  filtered_data <- reactive({
    data <- SO
    names(data) <- c("Sujeto Obligado", "Estado", "Nivel de Gobierno", "Tipo de SO", "Indice de GA", "Participacion", "Transparencia", "TG", "TC", "PG", "PC")
    #data <- subset(
    #  data,
    #  lifeExp >= input$life[1] & lifeExp <= input$life[2]
    #)
    
    #if (input$continent != "All") {
    #  data <- subset(
    #    data,
    #    continent == input$continent
    #  )
    #}
    data
  })
  
  
  
  output$BASE_DE_DATOS  <- DT::renderDT({
    data <- DT::datatable(filtered_data(),
                          #,
                          #, 
                          #initComplete = I('function(setting, json) { alert("done"); }'),
                          #backgroundColor = styleInterval(3.4, c('gray', 'yellow')),
                          options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'))) 
    #data 
  })
  
  # Create a download handler
  output$download_data <- downloadHandler(
    # The downloaded file is named "gapminder_data.csv"
    filename = "Datos_Metrica_2017.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  ###################################################### BASE_DE_DATOS
  
  
  
  ###################################################### PDF
  output$pdfview <- renderUI({
    tags$iframe(style="height:600px; width:100%", src="REx.pdf")
  })
  
  ###################################################### PDF
  
  
  
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
  
  ###################################################### renderTextSOseleccionados
  
  output$SOselect <- renderText({
       input$btnIncluir
    isolate({
      seleccion <- input$SELECTso
      paste("Has seleccionado", input$SELECTso)
      })
       
    })
  
  ###################################################### renderTextSOseleccionados
  
  ###################################################### observeMenuSelector
  
  observe({
    hei1<-input$SELECTedo
    hei2<-input$SELECTtipoSO
    choice1 <- SO$SO[SO$ts == hei2 & SO$edo == hei1]
    updateSelectInput(session, "SELECTso", choices=choice1, selected = choice1[1])
  })
  
  ###################################################### observeMenuSelector
  
  
  ################################################################################# Cm_1EDO
  output$Cm_1EDO <- renderPlotly({
    
    if(is.null(input$MAPA__shape_click$id)){
      dato <- subset.data.frame(dt, dt$num == 1)
    } else{
      i <- as.numeric(input$MAPA__shape_click$id) 
      dato <- subset.data.frame(dt, dt$num == i)
    }
    
    plot_ly(title = paste0("Informacion de estado de: ", dato$estado),   
              dato, x = dato$estado, y = dato$GA, type = 'bar', name = 'Gobierno Abierto')%>%
      add_trace(y = dato$P, name = 'Participacion', marker = list(color = 'rgb(110,44,18)'))%>%
      add_trace(y = dato$PG, name = 'Participacion desde el Gobierno', marker = list(color = 'rgb(183,70,29)')) %>%
      add_trace(y = dato$PC, name = 'Participacion desde el Ciudadano', marker = list(color = 'rgb(239,101,113)')) %>%
      add_trace(y = dato$T, name = 'Transparencia', marker = list(color = 'rgb(42,70,26)')) %>%
      add_trace(y = dato$TG, name = 'Transparencia desde Gobierno', marker = list(color = 'rgb(67,113,40)')) %>%
      add_trace(y = dato$TC, name = 'Transparencia desde Ciudadano', marker = list(color = 'rgb(133,200,123)')) %>%
      layout(yaxis = list(title = 'Valor Indice'), barmode = 'group', bargap=0.15, bargroupgap = 0.1) %>%
      config(displayModeBar = F)
    
  })
  
  ################################################################################# Cm_1EDO
  
  ###################################################### Tipo SO - ESTADO
  
  output$plotTIPOSO <- renderPlotly({
  
    if(is.null(input$MAPA__shape_click$id)){
      i <- 1
      aaa <- subset.data.frame(SO, edo == "Aguascalientes")
      
    } else{
      i <- as.numeric(input$MAPA__shape_click$id) 
      aaa <- subset.data.frame(SO, edo == dt$estado[dt$num == i])
    }
    
    aaa <- aaa %>% 
      group_by(ts) %>%
      summarize(mean_GA = mean(GA))
    
    a <-   ggplot(data = aaa, aes(x = ts, y = mean_GA, fill = ts)) + 
      ggtitle(paste0("Indice GA del estado de ", as.character(dt$estado[dt$num == i]))) + 
      labs(x = "Tipo de SO", y = "Indice de GA", colour = "Colour\nlegend") +
      geom_bar(stat="identity", show.legend = FALSE) + 
      theme_calc() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  
    ggplotly(a)
    
  })
  
  ###################################################### Tipo SO - ESTADO
  
  ################################################################################# RenderTexto
  output$rt <- renderText({
    
    d <- prom[c(1:10, 12:33),]
    d$num <- c(1:32)
    d <- d$estado
    
    as.character(d[input$MAPA__shape_click$id])
    
  })
  
  ################################################################################# RenderTexto
  
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
    y = as.character(prom$GA[str_detect(prom$estado, pattern = rebus::or1(rebus::exactly(seleccion)))])
    
    plot_ly(y=y, x=x, histfunc='sum', type = "histogram") %>%
      layout(yaxis=list(type='linear'))  %>%
      layout(autosize = T) 
  })
  ###################################################### barras_edo
  
  ###################################################### max_min_edo
  output$maxmin <- renderPlotly({
    seleccion <- c(input$g1, input$g2, input$g3, input$g4)
    
    SO_ <- SO
    
    SO_$dummy <- str_detect(SO_$edo, pattern = rebus::or1(rebus::exactly(seleccion)))
    
    SO_ <- SO_ %>%
      filter(dummy == TRUE)
    
    m_1 <- SO_ %>% 
      group_by(edo) %>%
      summarize(min_GA = min(GA), prom_GA = mean(GA), max_GA = max(GA)) %>%
      arrange(min_GA, prom_GA, max_GA)
    
   a <-  ggplot(SO_, aes(x = edo, y = GA)) +
      geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4) +
      geom_point(data = m_1, aes(x = edo, y = min_GA), colour = "blue", size = 5) +
      geom_point(data = m_1, aes(x = edo, y = prom_GA), colour = "green", size = 5) +
      geom_point(data = m_1, aes(x = edo, y = max_GA), colour = "orange", size = 5) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "")
   ggplotly(a)
  })
  
  ###################################################### max_min_edo
  
  ###################################################### comp_
  output$comp_ <- renderPlotly({
    seleccion <- c(input$g1, input$g2, input$g3, input$g4)
    print(seleccion)
    data <- prom
    plot_ly(data, 
      x = seleccion, 
        #prom$estado[str_detect(prom$estado, pattern = rebus::or1(rebus::exactly(seleccion)))] , 
      y = prom$P[str_detect(prom$estado, pattern = rebus::or1(rebus::exactly(seleccion)))], type = 'bar', name = 'Participacion') %>%
      add_trace(y = ~prom$T[str_detect(prom$estado, pattern = rebus::or1(rebus::exactly(seleccion)))], name = 'Transparencia') %>%
      layout(yaxis = list(title = 'Valor Indice'), xaxis = list(c(input$g1, input$g2, input$g3, input$g4)), barmode = 'group')
  })
  ###################################################### comp_
  
  
  ###################################################### NUBE_1
  output$plot3 <- renderImage({
    
    # Indice del selector
    z <- which(DR$SOS == input$SO)
    filename <- paste0(root,"www/clave", as.character(claves[z]), '.png')
    
    # Return a list containing the filename
    list(src = filename,
         contentType = "image/png", 
         width  = session$clientData$output_dona_1_width,
         height = session$clientData$output_dona_1_height,
         alt = "WORDCLOUD")
  }, deleteFile = FALSE)
  
  ###################################################### NUBE_1
  
  ###################################################### Tiempo de Respuesta
  
 # output$gphTiempoRespuesta <- renderPlotly({
#    
#    A <- ggplot(inai, aes(x = diferencia, y = Sujeto.Obligado)) + 
#      geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4) +
#      geom_point(data = tiempo_prom, aes(x = promedio, y = Sujeto.Obligado), colour = "yellow", size = 3) +
#      theme_solarized()
#    ggplotly(A)
#  })
  
  ###################################################### Tiempo de Respuesta
  
  
  
  
  
# final de server

})