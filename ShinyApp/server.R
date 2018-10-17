library(shiny)
library(dplyr)
library(MASS)
library(Hmisc)
library(lubridate)


shinyServer(function(input, output) {
  
    observeEvent(input$apply2, {
      showModal(modalDialog(
        title = "Resultados de la predicción",
        footer = modalButton("Cerrar"),
        easyClose = TRUE,
        fluidRow(column(10, align = "center", offset = 1,
                        output$tabla<-renderTable(
                          if(input$apply2){
                            daygen(input$fecha_inicial,input$fecha_final)
                          }
                        )
        ))
        
      ))
    })
  
  
  observeEvent(input$apply3, {
    showModal(modalDialog(
      title = "Resumen descriptivo de la predicción",
      footer = modalButton("Cerrar"),
      easyClose = TRUE,
      
                      output$grafico<-renderPlot(
                        if(input$apply3){
                          daydescri(input$fecha_inicial,input$fecha_final)
                        }
                      )
      
      
    ))
  })
})

