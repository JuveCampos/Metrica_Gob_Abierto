library(leaflet)


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
  setView(-101.11,22.04,  zoom = 4 )   %>% 
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