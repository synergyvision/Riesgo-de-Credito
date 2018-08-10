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
  
  
  output$estad1 <- renderDataTable({ 
    
    s <- input$file_data
    
    s1 <- read.table(s$datapath, header = input$header,
                     sep = input$sep, quote = input$quote)
    
    
    numcol <- dim(s1)[2]
    
    Posicion <- c("")
    for (i in 1:numcol) {
      Posicion[i+1] <- i
    }
    
    suma <- matrix(c("Minimo","Primer quartil","Mediana","Media","Tercer quartil","Maximo"),ncol = 6)
    
    for (i in 1:numcol) {
      suma <- rbind(suma,summary(s1[,i]))
    }
    
    suma <- cbind(Posicion,suma)
    
    
    
    
    as.data.frame(suma)
    
    
  })
  
  
  
  output$variables1 <- renderText({
    s <- input$file_data
    
    s1 <- read.table(s$datapath, header = input$header,
                     sep = input$sep, quote = input$quote)
    
    
    tamano <- 1:length(names(s1))
    
    paste(tamano,names(s1),sep = "-") 
  })
  
  output$varia23 <- renderText({
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
  
  output$accur <- renderTable({
    s <- input$file_data
    
    s1 <- read.table(s$datapath, header = input$header, sep = input$sep, quote = input$quote)
    
    if (input$radio2==1) {
      
      s1[,input$num] <- replace(s1[,input$num], s1[,input$num]==1,-1)
      s1[,input$num] <- replace(s1[,input$num], s1[,input$num]==0,1)
      s1[,input$num] <- replace(s1[,input$num], s1[,input$num]==-1,0)
      
    }
    
    
    
    ceros <- subset(s1, s1[,input$num]==0)
    unos <- subset(s1, s1[,input$num]==1)
    
    
    indices0 <- sample( 1:nrow( ceros ), nrow(ceros)*0.7 )
    ceros.muestreado <- ceros[ indices0, ]
    ceros.test <- ceros[-indices0,]
    
    indices1 <- sample( 1:nrow( unos ), nrow(unos)*0.7 )
    unos.muestreado <- unos[ indices1, ]
    unos.test <- unos[-indices1,]
    
    train <- rbind(ceros.muestreado,unos.muestreado)
    test <- rbind(ceros.test,unos.test)
    
    colnames(train)[input$num] <- "dependiente"
    colnames(test)[input$num] <- "dependiente"
    
    modelo <- glm(dependiente ~. , data = train, family = binomial(link = input$radio1))
    
    
    reduccion = step(modelo)
    
    pdata <- predict(reduccion, newdata = test, type = "response")
    
    pred <- confusionMatrix(data = as.factor(as.numeric(pdata>0.5)), reference = as.factor(test$dependiente))
    
    conf <- pred$table
    Valores <- c("Prediccion",0,1)
    Positivos <- c(0,conf[1,1],conf[1,2])
    Negativos  <- c(1,conf[2,1],conf[2,2])
    
    cbind(Valores,Positivos, Negativos)
    
    
  })
  
  
  output$roc <- renderPlot({
    
    
    
    s <- input$file_data
    
    s1 <- read.table(s$datapath, header = input$header, sep = input$sep, quote = input$quote)
    
    if (input$radio2==1) {
      
      s1[,input$num] <- replace(s1[,input$num], s1[,input$num]==1,-1)
      s1[,input$num] <- replace(s1[,input$num], s1[,input$num]==0,1)
      s1[,input$num] <- replace(s1[,input$num], s1[,input$num]==-1,0)
      
    }
    
    
    
    ceros <- subset(s1, s1[,input$num]==0)
    unos <- subset(s1, s1[,input$num]==1)
    
    
    indices0 <- sample( 1:nrow( ceros ), nrow(ceros)*0.7 )
    ceros.muestreado <- ceros[ indices0, ]
    ceros.test <- ceros[-indices0,]
    
    indices1 <- sample( 1:nrow( unos ), nrow(unos)*0.7 )
    unos.muestreado <- unos[ indices1, ]
    unos.test <- unos[-indices1,]
    
    train <- rbind(ceros.muestreado,unos.muestreado)
    test <- rbind(ceros.test,unos.test)
    
    colnames(train)[input$num] <- "dependiente"
    colnames(test)[input$num] <- "dependiente"
    
    modelo <- glm(dependiente ~. , data = train, family = binomial(link = input$radio1))
    
    
    reduccion = step(modelo)
    
    l <- roc(train$dependiente  ~ reduccion$fitted.values)
    plot(l,legacy.axes=T)
    
    
    
    
    
    
    
    
  })
  
  
  output$score <- renderDataTable({
    
    
    
    s <- input$file_data
    
    s1 <- read.table(s$datapath, header = input$header, sep = input$sep, quote = input$quote)
    
    if (input$radio2==1) {
      
      s1[,input$num] <- replace(s1[,input$num], s1[,input$num]==1,-1)
      s1[,input$num] <- replace(s1[,input$num], s1[,input$num]==0,1)
      s1[,input$num] <- replace(s1[,input$num], s1[,input$num]==-1,0)
      
    }
    
    
    
    ceros <- subset(s1, s1[,input$num]==0)
    unos <- subset(s1, s1[,input$num]==1)
    
    
    indices0 <- sample( 1:nrow( ceros ), nrow(ceros)*0.7 )
    ceros.muestreado <- ceros[ indices0, ]
    ceros.test <- ceros[-indices0,]
    
    indices1 <- sample( 1:nrow( unos ), nrow(unos)*0.7 )
    unos.muestreado <- unos[ indices1, ]
    unos.test <- unos[-indices1,]
    
    train <- rbind(ceros.muestreado,unos.muestreado)
    test <- rbind(ceros.test,unos.test)
    
    colnames(train)[input$num] <- "dependiente"
    colnames(test)[input$num] <- "dependiente"
    
    modelo <- glm(dependiente ~. , data = train, family = binomial(link = input$radio1))
    
    
    reduccion = step(modelo)
    
    
    Score <- predict(reduccion, newdata = test, type = "link")
    PD <- predict(reduccion, newdata = test, type = "response")
    cbind(1:1000,Score,PD)
    
    
    
    
    
    
  })
  
  datasetSelectr <- reactive({
    datasetSelectr <- reg
  })
  
  
  datasetInputr <- reactive({
   
    inFiler <- input$file_datar
    
    if (is.null(inFiler))
      return(NULL)
    read.table(inFiler$datapath, header = input$headerr,
               sep = input$sepr, quote = input$quoter)
    
  })
  
  
  data2 <- reactive({
    if(input$datasetr){
      data <- datasetSelectr()}
    
    else {
      data <- datasetInputr()
    }
  })
  
  ###Datos
  
  output$datatabler<-renderDataTable({
    data2()
  })
  
  output$dat <- renderTable({
    
    
    s <- input$file_data
    s1 <- read.table(s$datapath, header = input$header, sep = input$sep, quote = input$quote)
    
    if (input$radio2==1) {
      
      s1[,input$num] <- replace(s1[,input$num], s1[,input$num]==1,-1)
      s1[,input$num] <- replace(s1[,input$num], s1[,input$num]==0,1)
      s1[,input$num] <- replace(s1[,input$num], s1[,input$num]==-1,0)
      
    }
    
    
    
    ceros <- subset(s1, s1[,input$num]==0)
    unos <- subset(s1, s1[,input$num]==1)
    
    
    indices0 <- sample( 1:nrow( ceros ), nrow(ceros)*0.7 )
    ceros.muestreado <- ceros[ indices0, ]
    ceros.test <- ceros[-indices0,]
    
    indices1 <- sample( 1:nrow( unos ), nrow(unos)*0.7 )
    unos.muestreado <- unos[ indices1, ]
    unos.test <- unos[-indices1,]
    
    train <- rbind(ceros.muestreado,unos.muestreado)
    test <- rbind(ceros.test,unos.test)
    
    colnames(train)[input$num] <- "dependiente"
    colnames(test)[input$num] <- "dependiente"
    
    modelo <- glm(dependiente ~. , data = train, family = binomial(link = input$radio1))
    
    
    reduccion = step(modelo)
    
    s2 <- data2()
    
    Score <- predict(reduccion, newdata = s2, type = "link")
    PD <- predict(reduccion, newdata = s2, type = "response")
    d <- cbind(Score,PD)
    d
    
  })
  
  
})
