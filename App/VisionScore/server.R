
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
  
  D <- reactive({
    data1()[,input$idc]
  })
  
  ###Datos
  
  output$datatable<-renderDataTable({
   data1()
  })
  
  
  ###Estadísticas
  output$rendimientos <- renderPrint({
    if(is.null(data1())){return()}
    (D <-table(data1()$class))
    #datatable(D, options = list(dom = 't'),selection = list(target = 'column')) %>% formatCurrency(1:3, 'Bs. ', mark = '.', dec.mark = ',')
  })
  
  output$dataSelecta <- renderPrint({
    D()
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
