shinyServer(function(input, output) {

  # Almacenar Variables Reactivas
  RV <- reactiveValues()

  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
 
  datasetSelect <- reactive({
    datasetSelect <- mydata
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
  
  output$variables <- renderText({
    s <- input$file_data
    
    s1 <- read.table(s$datapath, header = input$header,
                                            sep = input$sep, quote = input$quote)
    
    
    tamano <- 1:length(names(s1))
    
    paste(tamano,names(s1),sep = "-") 
    })
  
    
  
  output$comparacion <- renderPlot({
    s <- input$file_data
    
    s1 <- read.table(s$datapath, header = input$header,
                     sep = input$sep, quote = input$quote)
    
    
    
    
    boxplot( s1[,input$num1]~ s1[,input$num])
    
  })
  
  
  output$estad1 <- renderTable({ 
    
    s <- input$file_data
    
    s1 <- read.table(s$datapath, header = input$header,
                     sep = input$sep, quote = input$quote)
    
    
    b <- as.data.frame(as.array(summary(s1[,1])))
    
    colnames(b) <- c(" "," ")
    b
    
    #s2 <- subset(s1, s1[,input$num1] )
    
    
    
    
    
    })
  
  
  
  output$variables1 <- renderText({
    s <- input$file_data
    
    s1 <- read.table(s$datapath, header = input$header,
                     sep = input$sep, quote = input$quote)
    
    
    tamano <- 1:length(names(s1))
    
    paste(tamano,names(s1),sep = "-") 
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
  
  
  
  
})
