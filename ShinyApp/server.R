library(shiny)
library(dplyr)
library(MASS)
library(Hmisc)
library(xlsx)
library(lubridate)


shinyServer(function(input, output) {
  output$tabla<-renderTable(
    if(input$apply2){
      daygen(input$fecha_inicial,input$fecha_final)
    }
  )
})

