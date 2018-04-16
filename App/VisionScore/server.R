#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(rriskDistributions)
library(DT)
library(VaRES)
library(rmarkdown)
library(dygraphs)
library(readr)
library(webshot)
library(rintrojs)
library(highcharter)

source("text.R")
source("conf.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  data <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file_data
    
    if (is.null(inFile))
      return(NULL)
    
    read.table(inFile$datapath, header = input$header,
               sep = input$sep, quote = input$quote)
  })
  
  
  ###Datos
  
  output$datatable<-renderDataTable({
    if(is.null(data())){return()}
    datatable(data()) %>% formatCurrency(1:3, 'Bs. ', mark = '.', dec.mark = ',')
  })
  
  
  ###Estadísticas
  
  output$estadisticas1 <- renderDataTable({
    if(is.null(data())){return()}
    D <- summary(data())
    datatable(D, options = list(dom = 't')) %>% formatCurrency(1:3, 'Bs. ', mark = '.', dec.mark = ',')
  })
})
