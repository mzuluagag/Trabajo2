library(shiny)
library(dplyr)
library(MASS)
library(Hmisc)
library(lubridate)
library(shinythemes)
load(file= "www/carritos",envir = .GlobalEnv,verbose = FALSE)
shinyUI(fluidPage(
  theme = "style.css",
  absolutePanel(class = "wrapper", 
                fluidRow(column(10, align = "center", offset = 1,  
                                h2("Predictor de registros de autos en el RUNT")
                                )),
                fluidRow(column(6, 
                                align = "center",
                                dateInput("fecha_inicial", 
                                          label = h3("Seleccione la fecha inicial"), 
                                          value = ""
                                          )
                                ),
                         column(6, 
                                align = "center",
                                dateInput("fecha_final", 
                                          label = h3("Seleccione la fecha final"), 
                                          value = ""
                                          )
                                )
                         ),
                fluidRow(column(12, actionButton("apply2",
                                                label = "Generar predicción", 
                                                class = "fadeIn fourth"
                                                )
                                )
                         ),
                fluidRow(column(12, actionButton("apply3",
                                                       label = "Resumen descriptivo",
                                                       class = "fadeIn descriptive")))
                )
))
