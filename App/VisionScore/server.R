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
library(CASdatasets)

source("text.R")
source("conf.R")
data("credit")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
 
   datasetSelect <- reactive({
   datasetSelect <- credit
  })
   
  datasetInput <- reactive({
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
  
  data1 <- reactive({
    if(input$dataset){
        data <- datasetSelect()}
      
         else {
      data <- datasetInput()
    }
  })
  ###Datos
  
  output$datatable<-renderDataTable({
   data1()
  })
  
  
  ###Estadísticas
  output$rendimientos <- renderDataTable({
    if(is.null(data1())){return()}
    D <-data1()
    datatable(D, options = list(dom = 't'),selection = list(target = 'column')) %>% formatCurrency(1:3, 'Bs. ', mark = '.', dec.mark = ',')
  })
  output$frecuencia <- renderDataTable({
    if(is.null(data1())){return()}
    D <-summary(data1())
    datatable(D, options = list(dom = 't')) %>% formatCurrency(1:3, 'Bs. ', mark = '.', dec.mark = ',')
  })
  
  output$estadisticas1 <- renderDataTable({
    if(is.null(data1())){return()}
    D <-summary(data1())
    datatable(D, options = list(dom = 't')) %>% formatCurrency(1:3, 'Bs. ', mark = '.', dec.mark = ',')
  })
})
