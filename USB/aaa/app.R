library(shiny)

row <- function(...) {
  tags$div(class="row", ...)
}

col <- function(width, ...) {
  tags$div(class=paste0("span", width), ...)
}

ui <- shinyUI(bootstrapPage(
  
  headerPanel("PDF VIEWER"),
  
  mainPanel(
    
    tags$div(
      class = "container",
      
      row(
        col(3, textInput("pdfurl", "PDF URL"))
      ),
      row(
        col(6, htmlOutput('pdfviewer')),
        col(6, tags$iframe(style="height:600px; width:100%", src="REx.pdf"))
      )
    )
  )
))



server <- shinyServer(function(input, output, session) {
  
  output$pdfviewer <- renderText({
    return(paste('<iframe style="height:600px; width:100%" src="', input$pdfurl, '"></iframe>', sep = ""))
  })
  
})

shinyApp(ui, server)