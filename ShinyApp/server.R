library(shiny)


shinyServer(function(input, output) {
  output$tabla<-renderText(
    if(input$apply2){
      month(input$fecha_inicial)
    }
  )
})

