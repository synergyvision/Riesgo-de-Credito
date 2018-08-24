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
  # 
  # datasetSelectr <- reactive({
  #   datasetSelectr <- reg
  # })
  
  
  # datasetInputr <- reactive({
  #   
  #   inFiler <- input$file_datar
  #   
  #   if (is.null(inFiler))
  #     return(NULL)
  #   read.table(inFiler$datapath, header = input$headerr,
  #              sep = input$sepr, quote = input$quoter)
  #   
  # })
  
  
  data1 <- reactive({
    if(input$dataset){
      data <- datasetSelect()}
    
    else {
      data <- datasetInput()
    }
  })
  
  # ###Datos
  # 
  # output$datatabler<-renderDataTable({
  #   data2()
  # })
  # 
  
  
  mod <- reactive(  {s1 <- data1()
                     
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
                     
                     return(reduccion)
                     
                     })
  
  
  
  
  output$variables <- renderText({

    s1 <- data1()
    
    
    tamano <- 1:length(names(s1))
    
    paste(tamano,names(s1),sep = "-") 
  })
  
  
  
  output$comparacion <- renderPlot({
  
    s1 <- data1()
    
    
    
    
    boxplot( s1[,input$num1]~ s1[,input$num])
    
  })
  
  
  output$estad1 <- renderDataTable({ 
    

    s1 <- data1()
    
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

    s1 <- data1()
    
    
    tamano <- 1:length(names(s1))
    
    paste(tamano,names(s1),sep = "-") 
  })
  
  output$varia23 <- renderText({

    s1 <- data1()
    
    
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

     s1 <- data1()
    # 
     if (input$radio2==1) {
    # 
       s1[,input$num] <- replace(s1[,input$num], s1[,input$num]==1,-1)
       s1[,input$num] <- replace(s1[,input$num], s1[,input$num]==0,1)
       s1[,input$num] <- replace(s1[,input$num], s1[,input$num]==-1,0)
     
     }
    # 
    # 
    # 
     ceros <- subset(s1, s1[,input$num]==0)
     unos <- subset(s1, s1[,input$num]==1)
    # 
    # 
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
    # 
    # modelo <- glm(dependiente ~. , data = train, family = binomial(link = input$radio1))
    # 
    # 
    # reduccion = step(modelo)
    
    pdata <- predict(mod(), newdata = test, type = "response")
    
    pred <- confusionMatrix(data = as.factor(as.numeric(pdata>0.5)), reference = as.factor(test$dependiente))
    
    conf <- pred$table
    Valores <- c("Prediccion",0,1)
    Positivos <- c(0,conf[1,1],conf[1,2])
    Negativos  <- c(1,conf[2,1],conf[2,2])
    
    cbind(Valores,Positivos, Negativos)
    
    
  })
  
  
  output$roc <- renderPlot({
    
    
    
   
    s1 <- data1()
    
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
    
    #modelo <- glm(dependiente ~. , data = train, family = binomial(link = input$radio1))
    
    
    #reduccion = step(modelo)
    
    reduccion <- mod()
    
    l <- roc(train$dependiente  ~ reduccion$fitted.values)
    plot(l,legacy.axes=T)
    
    
    
    
    
    
    
    
  })
  
  
  output$score <- renderDataTable({
    
    
    
    s1 <- data1()
    
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
    
    #modelo <- glm(dependiente ~. , data = train, family = binomial(link = input$radio1))
    
    
    reduccion = mod()
    
    
    Score <- predict(reduccion, newdata = s1, type = "link")
    PD <- predict(reduccion, newdata = s1, type = "response")
    n <- length(PD)
    cbind(1:n,Score,PD)
    
    
    
    
    
    
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
    
    
   
    s1 <- data1()
    
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
    
    #modelo <- glm(dependiente ~. , data = train, family = binomial(link = input$radio1))
    
    
    reduccion = mod()
    
    s2 <- data2()
    
    Score <- predict(reduccion, newdata = s2, type = "link")
    PD <- predict(reduccion, newdata = s2, type = "response")
    d <- cbind(Score,PD)
    d
    
  })
  
  
  score1 <- reactive({
    s1 <- data1()
    
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
    
    
    
    
    reduccion = mod()
    
    s2 <- data1()
    
    
    PD <- predict(reduccion, newdata = s2, type = "response")
    return(cbind(PD))
    
    
    
    
  })
  
  
  pdPropias <- reactive({
    
    inFiler <- input$file_datar1
    
    if (is.null(inFiler))
      return(NULL)
    read.table(inFiler$datapath, header = input$headerr1,
               sep = input$sepr1, quote = input$quoter1)
    
  })
  
  
  data3 <- reactive({
    if(input$datasetr1){
      
      
      score1()
      
      
      }
    
    else {
      data <- pdPropias()
    }
  })
  
  ###Datos
  
  output$datatabler1<-renderDataTable({
    data3()
  })
  
  
  output$var <- renderText({
   
    # Primero necesitamos las espocisiones al default
    
    s1 <- data1()
    
    
    
    
    ###supondremos que son activos sin lineas extra 
    ### en este caso la exposicion coincide con el saldo
    
    ##exposicion de la cartera
    #View(mydata)
    
    
    EAD <- s1[,"Credit.Amount"]
    
    ###supondremos que la perdida dado el default es la misma para toda  la cartera
    ### la institucion puede ajustar a un cliente en particular una perdida diferente
    
    LGD <- (100-input$uni)/100
    
    
    DP= data3()
    
    ####Calculamos ahora la perdida esperada
    
    EL <- EAD*LGD*DP
    
    
    ### Se calcula la expocicion que se espera érder en caso de default
    
    Ei <- EAD*LGD
    
    #### Se escoge una unidad de perdida
    
    E <- input$uniper
    
    
    
    
    ###### se calculan las unidades de perdida
    
    v <- Ei/E
    
    e <- EL/E
    
    ###se calcula el paramtro de poisson
    ### correspondiente a la posibilidad de incumplimiento
    
    lambda <- - log(1-DP)
    
    
    ###creando las bandas
    
    L <- ceiling(v)
    
    
    
    bandas <- list()
    
    for (k in 1:range(L)[2]) {
      
      
      bandas[[k]] <- which(L==k)
      
    }
    
    
    ###se calculan los parametros de poisson por banda
    
    lambdaj <- numeric(range(L)[2])
    
    for (k in 1:range(L)[2]) {
      lambdaj[k] <- sum(lambda[bandas[[k]]])
    }
    
    
    
    #calculamos la perdida esperada por banda
    
    ei <- lambdaj*1:length(bandas)
    
    ###factor de ajuste
    
    gamm <- numeric(length(Ei)) 
    
    
    for(i in 1:length(lambdaj)){
      
      
      gamm[bandas[[i]]] <- Ei[bandas[[i]]]/(i*E)
      
    }
    ####Numero de incumplimientos de toda la cartera
    
    
    
   
    IncCar <- sum(lambdaj)
    
    
    
    ### Probabilides de unidades de perdida de toda la cartera
    
    p0 <- exp(-IncCar)
    
    
    probandas <- numeric(10000)
    
    probandas[1] <- p0
    
    probandasc <- probandas
    
    length(ei)
    eii <- numeric(10000)
    
    eii[1:length(bandas)] <- ei[1:length(bandas)]
    #View(eii)
    for (i in 2:10000) {
      
      probandas[i] <- sum(probandasc[1:i-1]*rev(eii[1:i-1]))/(i-1)
      probandasc <- probandas
    }
    
    #View(probandas)ç
    
    
    sum(probandas[1:10000])
    
    
    acum <- c()
    
    for (l in 1:10000) {
      acum[l] <- sum(probandas[1:l]) 
    }
    
    #####Var
    min(which(acum > (as.numeric(input$conf)/100)))*E
    
    
     
    
    
    
    
    })
})





