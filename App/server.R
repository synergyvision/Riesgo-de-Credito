shinyServer(function(input, output, session) {
  
  
 
  ####### CrediRisk+#######################
  
        ############ Primera seccion (Datos )############
  
 
        ############# Parte que se encarga de leer los datos cargados por el usuario
  
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
  
  
  
   ####### Datos de ejemplo de una institucion financiera alemana###
  
  datasetSelect <- reactive({
    datasetSelect <- mydata
  })
  
  
              ###### Cargando datos con que se trabajara: entre los de ejemplo y los propios
        
  data1org <- reactive({
    if(input$dataset){
      data <- datasetSelect()}
    
    else {
      data <- datasetInput()
    }
  })
  
  
  ####Se muestran los datos
  
  
  output$datatable<-renderDataTable({
    data1org()
  },options = list(scrollX=T,scrollY=300))
  
  
  
  
  
  
  
  ############# Parte que se encarga de leer los datos cargados por el usuario
  
  datasetInput_Cla_Cr <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file_data_Cla_Cr
    
    if (is.null(inFile))
      return(NULL)
    read.table(inFile$datapath, header = input$header_Cla_Cr,
               sep = input$sep_Cla_Cr, quote = input$quote_Cla_Cr)
    
  })
  
  
  
  ####### Datos de ejemplo de una institucion financiera alemana###
  
  datasetSelect_Cla_Cr <- reactive({
    datasetSelect <- clasecrm()
  })
  
  
  ###### Cargando datos con que se trabajara: entre los de ejemplo y los propios
  
  data_Cla_Cr <- reactive({
    if(input$dataset_Cla_Cr){
      data <- datasetSelect_Cla_Cr()}
    
    else {
      data <- datasetInput_Cla_Cr()
    }
  })
  
  
  ####Se muestran los datos
  
  
  output$datatable_Cla_Cr<-renderDataTable({
    data_Cla_Cr()
  },options = list(scrollX=T,scrollY=300))
  
  
  
  
  
  
  
  ##### OutVar Se encarga de obtener los nombres de la variables 
  
  outVar = reactive({
    mydata = data1org()
    names(mydata)
  })  
  
  ### Se actuliza el input column1 con la variable anterior
  
  observe({
    updateSelectInput(session, "columns",
                      choices = outVar()
    )}) 
  
  
  
  
  
 ########### Segunda seccion (Estadisticos)
  
    ##### Relacion de las variables independientes
                
  
        ##### OutVar1 Se encarga de obtener los nombres de la variables menos la variable de estudio
        
  outVar1 = reactive({
    
    nombres <- colnames(data1org())
    
    nombre <- input$columns
    
    posi <- which(nombres == nombre)
    
    mydata = data1org()
    
    c <- names(mydata)
    
    c[-posi]
    
    
  })  
  
  
      
      ### Se actuliza el input column1 con la variable anterior
  observe({
    updateSelectInput(session, "columns1",
                      choices = outVar1()
    )})
  
        ################# grafica que compara las variables
  
      
  
                            ### funcion que se encarga de graficar
  grafica <- function(datos,nom,nom2){
    
    s1 <- datos
    nombres <- colnames(datos)
    
    nombre <- nom
    
    posi <- which(nombres == nombre)
    
    nombre1 <- nom2
    
    posi1 <- which(nombres == nombre1)
    
    s1[,posi]<-  as.factor(s1[,posi])
    
    
    
    
    p10 <- ggplot(s1, aes(x = s1[[nombre]], y = s1[[nombre1]])) +
      geom_boxplot(fill = "#56B4E9") +
      scale_y_continuous(name = "Escala de valores") +  scale_x_discrete(name = "Categorias") +
      ggtitle("Comparación entre las categorias de la variable seleccionada") 
    return(p10)
    
    
    
  }
  
  
                        # Aplicando la funcion anterior
  output$comparacion <- renderPlotly({
    
    input$goButton
    
    ca7 <- try(isolate(ggplotly(grafica(data1org(),input$columns,input$columns1))))
    
    
    if (class(ca7)=="try-error") {
      
      df <- data.frame()
      ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
      
    }else{ca7}
    
    
    
  })
  
                ####### Funcion que recibe los datos y muestra la informacion etadistica
  
  estadf<-function(datos,nomb){
    
    s1 <- datos
    
    
    
    s2 <- rbind(summary(s1[[nomb]]))
    colnames(s2) <- c("Mínimo","Primer Quartil", "Mediana","Media", "Tercer Quartil", "Máximo")
    
    return(s2)
    
  }
  
  #### Aplicando la funcion anterioe para mostrar la informacion de la variable seleccionada
  
  output$estad1 <- renderDataTable({ 
    
    ca8 <- try(estadf(data1org(),input$columns1))
    
    if (class(ca8)=="try-error") {
      
      "Cargue datos"
      
    }else{ca8}
    
    
    
    
    
  })
    
        ####  subseccion Seleccion de variables
  
      
      ### Se calculan los p-valores de las variables cualitativas con la funcion pval
 
   
   
  pval <- function(datos){
    
    
    D<- datos
    M <- c()
    
    
    for (i in 2:length(names(datos))) {
      
      if(length(summary(as.factor(D[[i]])))<=10){
        M[length(M)+1] <- i
        
      }
      
    }
    
    
    D1 <- D[,c(1,M)]
    
    
    pval <- NULL
    est <- NULL
    rec <- NULL
    nomb <- colnames(D1)
    
    for (i in 2:length(colnames(D1))) {
      
      df <- D1[,c(1,i)]
      df1 <- dummy_cols(df,select_columns = nomb[i])
      
      d0 <- subset(df1,Creditability==0)
      d1 <- subset(df1,Creditability==1)
      
      d0 <- apply(d0, 2, sum)
      d1 <- apply(d1, 2, sum)
      
      d <- data.frame(t(data.frame(d0,d1)))
      
      d <-d[,-2]
      
      
      d$Creditability[2] <- 1
      
      d$Creditability[2] <- "buenos"
      d$Creditability[1] <- "malos"
      
      
      nombre <- d$Creditability
      rownames(d) <- nombre
      d <- d[,-1]
      
      pr <- chisq.test(d)
      pval[i] <- round(pr$p.value,4)
      est[i] <-round(pr$statistic,4)
      rec[i] <- round(qchisq(1-as.numeric(input$significancia),pr$parameter),4)
    }
    
    
    pval <- t(pval)
    est <- t(est)
    rec[i]
    
    vd <- nomb[which(pval > 0.05)]
    
    
    j <- colnames(datos)
    
    
    
    inf <- rbind(pval,est,rec)
    inf[1,1] <- "P-valor"
    inf[2,1] <- "Estadistico"
    inf[3,1] <- "Valor Crítico"
    
    colnames(inf) <- nomb
    colnames(inf)[1] <- ""
    return(inf)
  }
  
  ## se calculan los pvalores
   
  pvalor <- reactive({pval(data1org())})
   
  
   
  ## seccion que muestra los p-valores de las variables cualitativas
   
   
  output$datatablecu <- renderDataTable({
    
    input$goButton1
    
    ca10 <- try(isolate(pvalor()))
    if (class(ca10)=="try-error") {
      
      "Cargue datos y seleccione parametros"
    }else{ca10}
    
    
  })
  
  
  ### Se calculan los p-valores de las variables cuantitativas con la funcion pval.
  
  pval. <- function(datos){
    
    
    D<- datos
    M <- c()
    
    
    for (i in 2:length(names(datos))) {
      
      if(length(summary(as.factor(D[[i]])))<=10){
        M[length(M)+1] <- i
        
      }
      
    }
    
    D1 <- as.data.frame(D[,-M])
    pval <- NULL
    est <- NULL
    rec <- NULL
    
    nomb <- colnames(D1)
    
    
    for (i in 2:length(nomb)) {
      
      df1 <- D1[,c(1,i)]
      
      d0 <- subset(df1, Creditability==0)
      d1 <- subset(df1, Creditability==1)
      
      p1 <- d0[[2]]
      p2 <- d1[[2]]
      
      
      w <-  ks.test(p1,p2)
      
      pval[i] <- round(w$p.value,4)
      est[i] <- round(w$statistic,4)
      
      
    }
    
    pval <- t(pval)
    est <- t(est)
    rec <- pval
    
    
    inf <- rbind(pval,est,rec)
    colnames(inf)<-nomb
    inf[1,1] <- "P-valor"
    inf[2,1] <- "Estadistico"
    inf[3,1] <- "Valor Crítico"
    colnames(inf)[1] <- ""
    return(inf)
    
    
  }
  
  
  #### En la variable data1. se eliminan las variables cualitativas que tengan 
  #### un nivel de significancia mayor al del deseado por el usuarios el nivel esta en la variable input$significancia
  
  data1. <- reactive({
    
    
    datos <- data1org()
    
    pvalores <- pvalor()[1,]
    
    pvalores <- pvalores[-1]
    
    pvalores1 <- as.matrix(pvalores)
    
    pvalores1 <- as.vector(pvalores1)
    
    names(pvalores1) <- names(pvalores)
    
    filtro <- pvalores1[which(pvalores1 > as.numeric(input$significancia))]
    
    vd <- names(filtro)
    
    j <- colnames(datos)
    
    
    
    final <- datos[, !(j %in% vd)]
    return(final)
    
  })
  
  
  #####Calculo de pvalores de la variable cuantitativa
  pvalor1 <- reactive({pval.(data1.())})
  
  
  ## seccion que muestra los p-valores de las variables cuantitativas
  
  
  
  output$datatablecu1 <- renderDataTable({
    
    input$goButton2
    
    ca11 <- try(isolate(pvalor1()))
    if (class(ca11)=="try-error") {
      
      "Cargue datos y seleccione parametros"
    }else{ca11}
    
  })
 
   
  
  ### Se eliminan las variables cuantitativas con un nivel de significancia mayor al del requerido por el usuario
  
  

  
 data1 <- reactive({
   
   datos <- data1.()
   
   pvalores <- pvalor1()[1,]
   
   pvalores <- pvalores[-1]
   
   pvalores1 <- as.matrix(pvalores)
   
   pvalores1 <- as.vector(pvalores1)
   
   names(pvalores1) <- names(pvalores)
   
   filtro <- pvalores1[which(pvalores1 > as.numeric(input$significancia1))]
   
   vd <- names(filtro)
   
   j <- colnames(datos)
   
   
   
   final <- datos[, !(j %in% vd)]
   return(final)

    })
 
 
 
 ########### Seccion perdida por incumplimiento.
 
 
 ####Se cargan los datos de la manera usual
 
 datasetSelectrl <- reactive({
   datasetSelectrl <- perdidas
 })
 
 
 datasetInputrl <- reactive({
   
   inFilerl <- input$file_datarl
   
   if (is.null(inFilerl))
     return(NULL)
   read.table(inFilerl$datapath, header = input$headerrl,
              sep = input$seprl, quote = input$quoterl)
   
 })
 
 data7 <- reactive({
   if(input$datasetrl){
     data <- datasetSelectrl()}
   
   else {
     data <- datasetInputrl()
   }
 })
 
 ### esta parte se encarga de mostrar los datos
 
 output$datatablerl<-renderDataTable({
   data7()
 },options = list(scrollX=T,scrollY=300))
 
 
 
 ######## lgdq se encarga de hacer la grafica
 
 lgd1 <- reactive({
   
   lgd <-data7()["Perdidas"]
   
   p7 <- ggplot(lgd, aes(x = Perdidas)) +
     geom_histogram()+labs(x = "Pérdida",y = "Frecuencia")
   

   
   return( ggplotly(p7))
   
 })
 
 ### aqui se muestra la grafica.
 
 output$curvalgd <- renderPlotly({
   
   ca13 <- try(lgd1())
   
   
   if (class(ca13)=="try-error") {
     
     df <- data.frame()
     ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
     
   }else{ca13}
 
   
 })
 
 ### Lgd con bootstrap
 
 
 bootL <- function(datos,n){
   
   
   medias <- NULL
   
   for ( i  in 1:n) {
     muestra <- sample(datos$Perdidas,ceiling((as.numeric(input$bootT)/100)*length(datos$Perdidas)),replace = T)
     
     medias[i] <- mean(muestra)
     
   }
   
   medias <- as.data.frame(medias)
   
   colnames(medias) <- c("Perdidas")
   
   
   p7 <- ggplot(medias, aes(x = Perdidas)) +
     geom_histogram()+labs(x = "Pérdida",y = "Frecuencia")
   
   
   p7 <- ggplotly(p7)
   promedio <- mean(medias[,1])
   desv <- sd(medias[,1])
   lista <- list(p7,promedio,desv)
   return(lista)
   

 }
 
 
 ### aqui se muestra la grafica.
 
 bot <- reactive({
   
   input$goButton5
   
   isolate(bootL(data7(),input$boot))
           })
 
 ############
 
 output$booot1 <- renderPlotly({
   
  
   
   ca9879 <- try(bot()[[1]])
   
   
   if (class(ca9879)=="try-error") {
     
     df <- data.frame()
     ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
     
   }else{ca9879}
   
   
 })
 
 
 
 output$boots3 <- renderUI({
   my_calculated_value <- round(as.numeric(bot()[[2]])*100,2)
   withMathJax(paste0("El valor promerdio de perdida cuando un cliente esta Default es: $$", my_calculated_value,"$$"))
 })
 
 output$boots4 <- renderUI({
   media <- round(as.numeric(bot()[[2]])*100,2)
   des <- round(as.numeric(bot()[[3]])*100,2)
   
   withMathJax(paste0("El intevalo de confianza al " ,input$boot23,"% para este valor : $$(", round(media+(qnorm((1-(as.numeric( input$boot23)/100 ))/2))*des/sqrt(as.numeric(input$boot)),2),",", round(media-(qnorm((1-(as.numeric( input$boot23)/100 ))/2)*des/sqrt(as.numeric(input$boot))),2),")$$"))
 }) 
 
###### Seccion score de credito
 
 
#############sub seccion seleccion y resultdos del modelo
 
 
 
 ###############Funcion que crea el modelo para el score
 
 modprueba <- function(datos,datos2,nom,linkm)  {
   
   s1 <- datos
   
   nombres <- colnames(datos2)
   
   nombre <- nom
   
   posi <- which(nombres == nombre)
   

   ceros <- subset(s1, s1[,posi]==0)
   unos <- subset(s1, s1[,posi]==1)
   
   
   indices0 <- sample( 1:nrow( ceros ), nrow(ceros)*0.7 )
   ceros.muestreado <- ceros[ indices0, ]
   ceros.test <- ceros[-indices0,]
   
   indices1 <- sample( 1:nrow( unos ), nrow(unos)*0.7 )
   unos.muestreado <- unos[ indices1, ]
   unos.test <- unos[-indices1,]
   
   train <- rbind(ceros.muestreado,unos.muestreado)
   test <- rbind(ceros.test,unos.test)
   
   colnames(train)[posi] <- "dependiente"
   colnames(test)[posi] <- "dependiente"
   
   modelo <- glm(dependiente ~. , data = train, family = binomial(link = linkm))
   
   
   reduccion = step(modelo)
   
   return(reduccion)
   
 }
 
 GlmModel <- reactive({
   
   input$goButton3
   
   isolate(modprueba(data1(),data1org(),input$columns,input$radio1))
   
 }
   )
 
 
 #########Rating
 
 ############# Parte que se encarga de leer los datos cargados por el usuario
 
 datasetInputRat <- reactive({
   # input$file1 will be NULL initially. After the user selects
   # and uploads a file, it will be a data frame with 'name',
   # 'size', 'type', and 'datapath' columns. The 'datapath'
   # column will contain the local filenames where the data can
   # be found.
   
   inFile <- input$file_dataRat
   
   if (is.null(inFile))
     return(NULL)
   read.table(inFile$datapath, header = input$headerRat,
              sep = input$sepRat, quote = input$quoteRat)
   
 })
 
 
 
 ####### Datos de ejemplo de una institucion financiera alemana###
 
 datasetSelectRat <- reactive({
   datasetSelect <- rat
 })
 
 
 ###### Cargando datos con que se trabajara: entre los de ejemplo y los propios
 
 dataRat <- reactive({
   if(input$datasetRat){
     data <- datasetSelectRat()}
   
   else {
     data <- datasetInputRat()
   }
 })
 
 ####Se muestran los datos
 
 
 output$datatableRat<-renderDataTable({
   dataRat()
 },options = list(scrollX=T,scrollY=300))
 
 
 ####### función para calcular el modelo de Rating
 
 lda_mo <- function(dat){
   
   ind <- as.data.frame(as.numeric(dat[,1]))
   colnames(ind)<- c("PD") 
   
   d <- lda(dat[,2]~ .,data = ind)
   return(d)
   
 }
 
 ###########Funcion que calcula la informacion del modelo
 
 infLda <- function(lda){
   
   prior <- round(lda$prior,4)
   mea <- t(round(lda$means,4))
   inf <- rbind(prior,mea)
   inf <- cbind(c("Probabilidades a Priori","Valores Medio de los Grupos"),inf)
   return(inf)
 }
 
##### Se calcula el modelo
 
 mod_rat <- reactive(lda_mo(dataRat()))
 
 ### Se muestra la informa del modelo
 
 output$datatableRatInf <- renderDataTable({
   
   ca1342 <- try(infLda(mod_rat()))
   
   if (class(ca1342)=="try-error") {
     
     "Cargue datos"
     
   }else{ca1342}
 })
 
 
 ############# Aqui se calcula el rating de nuevos clientes
 
 
 ############# Parte que se encarga de leer los datos cargados por el usuario
 
 datasetInputRatN <- reactive({
   # input$file1 will be NULL initially. After the user selects
   # and uploads a file, it will be a data frame with 'name',
   # 'size', 'type', and 'datapath' columns. The 'datapath'
   # column will contain the local filenames where the data can
   # be found.
   
   inFile <- input$file_dataRatN
   
   if (is.null(inFile))
     return(NULL)
   read.table(inFile$datapath, header = input$headerRatN,
              sep = input$sepRatN, quote = input$quoteRatN)
   
 })
 
 
 
 ####### Datos del score###
 
 datasetSelectRatN <- reactive({
   datasetSelect <- proyec()
   
 })
 
 
 ###### Cargando datos con que se trabajara: entre los de ejemplo y los propios
 
 dataRatN <- reactive({
   if(input$datasetRatN){
     data <- datasetSelectRatN()}
   
   else {
     data <- datasetInputRatN()
   }
 })
 
 
 
 #### Funcion que calcula el rating a partir  del  modelo
 
 
 Rat_Cli <- function(d,df){
   
   pd <- as.data.frame(as.numeric(df[,1]))
   colnames(pd) <- c("PD")
   rat <- predict(d,pd)
   rat_cli <- as.data.frame(rat$class)
   colnames(rat_cli) <- c("Rating")
   return(rat_cli)
   
 }
 
 proyrat <- reactive({
   
   l1 <- as.data.frame(dataRatN()[,3])
   colnames(l1) <- c("Probabilidad de incumplimiento")
   
   
   l2 <- Rat_Cli(mod_rat(),as.data.frame(dataRatN()[,3]))
   
   
   l3 <- cbind(l1,l2)
   l3
   
 })
 
 output$datatableRatNCF <- renderDataTable({

   proyrat()
   
   
   
 },options = list(scrollX=T,scrollY=300))
 
 
 output$download2 <- downloadHandler(
   filename = function(){"score.csv"}, 
   content = function(fname){
     write.csv(proyrat(), fname)
   }
 )
 
 #####Aque se calcula la matriz de confusion del modelo
 
 calaccur <- reactive(
   {
     s1 <- data1()
     
     nombres <- colnames(data1org())
     
     nombre <- input$columns
     
     posi <- which(nombres == nombre)
     
  
     
     ceros <- subset(s1, s1[,posi]==0)
     unos <- subset(s1, s1[,posi]==1)
     
     indices0 <- sample( 1:nrow( ceros ), nrow(ceros)*0.7 )
     ceros.muestreado <- ceros[ indices0, ]
     ceros.test <- ceros[-indices0,]
     
     indices1 <- sample( 1:nrow( unos ), nrow(unos)*0.7 )
     unos.muestreado <- unos[ indices1, ]
     unos.test <- unos[-indices1,]
     
     train <- rbind(ceros.muestreado,unos.muestreado)
     test <- rbind(ceros.test,unos.test)
     
     colnames(train)[posi] <- "dependiente"
     colnames(test)[posi] <- "dependiente"
     
     pdata <- predict(modprueba(data1(),data1org(),input$columns,input$radio1), newdata = test, type = "response")
     
     pred <- confusionMatrix(data = as.factor(as.numeric(pdata>0.5)), reference = as.factor(test$dependiente))
     
     conf <- pred$table
     Valores <- c("Prediccion",0,1)
     Positivos <- c(0,conf[1,1],conf[1,2])
     Negativos  <- c(1,conf[2,1],conf[2,2])
     
     return(cbind(Valores,Positivos, Negativos))
     
   }
 )
 
 
 #######Se muestra la matriz de confusion.
 
 
 output$accur <- renderTable({
   
   ca14 <- try(calaccur())
   
   if (class(ca14)=="try-error") {
     
     "Cargue datos"
     
   }else{ca14}
   
   
   
   
 })
 
 
 ############# Funcion que calcula la curva roc
 
 calroc <-function(dat,dat1,colum,modelos){
   
   s1 <- dat
   nombres <- colnames(dat1)
   
   nombre <- colum
   
   posi <- which(nombres == nombre)
   
   ceros <- subset(s1, s1[,posi]==0)
   unos <- subset(s1, s1[,posi]==1)
   
   
   indices0 <- sample( 1:nrow( ceros ), nrow(ceros)*0.7 )
   ceros.muestreado <- ceros[ indices0, ]
   ceros.test <- ceros[-indices0,]
   
   indices1 <- sample( 1:nrow( unos ), nrow(unos)*0.7 )
   unos.muestreado <- unos[ indices1, ]
   unos.test <- unos[-indices1,]
   
   train <- rbind(ceros.muestreado,unos.muestreado)
   test <- rbind(ceros.test,unos.test)
   
   colnames(train)[posi] <- "dependiente"
   colnames(test)[posi] <- "dependiente"
   
   
   reduccion <- modelos
   
   l <- roc(train$dependiente  ~ reduccion$fitted.values)
   return(l)
 }
 
 
 
 #### Donde se muetra la curva ROC
 
 output$roc <- renderPlot({
   
   ca15 <- try(ggroc(calroc(data1(),data1org(),input$columns,modprueba(data1(),data1org(),input$columns,input$radio1)),legacy.axes=T))
   
   
   if (class(ca15)=="try-error") {
     
     df <- data.frame()
     ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
     
   }else{ca15}
   
   
   
   
 })
 
 ###########Se muestran los parametros de la regresión
 coefglm <- function(modelo){
   
   l1 <- modelo$coefficients
   
   l2 <-names(modelo$coefficients)
   
   res <- t(rbind(l2,l1))
   
   res[1,1] <- "Punto de corte con el eje Y"
   colnames(res) <- c("Variables","Coeficientes")
   
   return(res)
   
 }
 
 output$coefglm <- renderDataTable({
   
   
   
   
   ca134 <- try(coefglm(GlmModel()))
   if (class(ca134)=="try-error") {
     
     "Cargue datos"
   }else{ca134}
   
   
 })
 #####Se muestra información estadistica del modelo
 
 
 estglm <- function(modelo){
   
   aic <- round(modelo$aic,2)
   
   ND <- round(modelo$null.deviance,2)
   
   RD <- round(modelo$deviance,2)
   
   fis <- modelo$iter
   
   Est <- c(aic,RD,ND,round(fis,1))
   
   inf <- data.frame(c("Criterio de información de Akaike","Desviación de los residuos","Desviación Nula","Número de iteraciones de Fischer"),Est)
   colnames(inf) <- c("Estadísticos","Resultado")
   
   return(inf)
   
 }
 
 
 output$estglm <- renderDataTable({
   
   ca139 <- try(estglm(GlmModel()))
   if (class(ca139)=="try-error") {
     
     "Cargue datos"
   }else{ca139}
   
   
 })
 

 ############### Subseccion Score de la cartera de credito de entrenamento
 
 ###########Funcion que calcula el score y pd de toda la cartera
 
 scor <- function() {
   
   
   s1 <- data1()
   nombres <- colnames(data1org())
   
   nombre <- input$columns
   
   posi <- which(nombres == nombre)
   
   
   reduccion = modprueba(data1(),data1org(),input$columns,input$radio1)
   
   
   Score <- predict(reduccion, newdata = s1, type = "link")
   PD <- predict(reduccion, newdata = s1, type = "response")
   n <- length(PD)
   ress <- cbind(1:n,Score,PD)
   colnames(ress) <- c("Posición","Score","Probabilidad de incumplimiento") 
   return(ress)
   
 }
 
 ######Se muestra el score de toda la cartera
 
 
 
 output$score <- renderDataTable({
   
   ca16 <- try(scor())
   if (class(ca16)=="try-error") {
     
     "Cargue datos"
   }else{ca16}

   
 },options = list(scrollX=T,scrollY=300))
 

 output$download <- downloadHandler(
   filename = function(){"score.csv"}, 
   content = function(fname){
     write.csv(scor(), fname)
   }
 )
 ######## subcesion Proyeccion a nuevos clientes
 
 #### Se cargan los datos de la manera usual
 
 
 datasetSelectr <- reactive({
   datasetSelectr <- reg
 })
 
 
 
 datasetInputproy <- reactive({
   
   inFiler <- input$file_dataproy
   
   if (is.null(inFiler))
     return(NULL)
   read.table(inFiler$datapath, header = input$headerproy,
              sep = input$sepproy, quote = input$quoteproy)
   
 })
 
 
 dataaa2 <- reactive({
   if(input$datasetr){
     data <- datasetSelectr()
     
     }
   
   else if(input$userFiler) {
     
     data <- datasetInputproy()
   }
 })
 
 
 ### Se muestran los datos
 
 
 output$datatabler<-renderDataTable({
   dataaa2()
 })
 
 
 
 
 
   proyec <- reactive({
     if(input$datasetr ){
     
     
     s1 <- dataaa2()
     nombres <- colnames(data1org())
     
     nombre <- input$columns
     
     posi <- which(nombres == nombre)
     
     
     reduccion = GlmModel()
     
     
     Score <- predict(reduccion, newdata = s1, type = "link")
     PD <- predict(reduccion, newdata = s1, type = "response")
     n <- length(PD)
     ress <- cbind(1:n,Score,PD)
     colnames(ress) <- c("Posición","Score","Probabilidad de incumplimiento") 
     return(ress)
     
     }else if(input$userFiler & !is.null(input$file_dataproy)){
       
       
       s1 <- dataaa2()
       nombres <- colnames(data1org())
       
       nombre <- input$columns
       
       posi <- which(nombres == nombre)
       
       
       reduccion = modprueba(data1(),data1org(),input$columns,input$radio1)
       
       
       Score <- predict(reduccion, newdata = s1, type = "link")
       PD <- predict(reduccion, newdata = s1, type = "response")
       n <- length(PD)
       ress <- cbind(1:n,Score,PD)
       colnames(ress) <- c("Posición","Score","Probabilidad de incumplimiento") 
       return(ress)
       
       
     }
       
       
       
     
   
 })
 
 
 
 
 output$proy <- renderDataTable({
   
   pro <- try(proyec())
   if (class(pro)=="try-error") {
     
     "Cargue datos"
   }else{pro}
   
   
 },options = list(scrollX=T,scrollY=300))
 
 
 
 output$download1 <- downloadHandler(
   filename = function(){"score.csv"}, 
   content = function(fname){
     write.csv(proyec(), fname)
   }
 )
  
  ###### Seccion Parametros y resultados
 
 
 ### Subseccion Parametros iniciales   
 
 ### Se cargan los datos de la manera usual
 
 
 pdPropias <- reactive({
   
   inFiler <- input$file_datar1
   
   if (is.null(inFiler))
     return(NULL)
   read.table(inFiler$datapath, header = input$headerr1,
              sep = input$sepr1, quote = input$quoter1)
   
 })
 
 
 ###### En caso de que el usuario decida usar el modelo
 ###### se calculas las probabilidades de incumplimiento con score1()
 
 score1 <- reactive({
   s1 <- data1()
   nombres <- colnames(data1org())
   
   nombre <- input$columns
   
   posi <- which(nombres == nombre)
   
   
   
   
   
   ceros <- subset(s1, s1[,posi]==0)
   unos <- subset(s1, s1[,posi]==1)
   
   
   indices0 <- sample( 1:nrow( ceros ), nrow(ceros)*0.7 )
   ceros.muestreado <- ceros[ indices0, ]
   ceros.test <- ceros[-indices0,]
   
   indices1 <- sample( 1:nrow( unos ), nrow(unos)*0.7 )
   unos.muestreado <- unos[ indices1, ]
   unos.test <- unos[-indices1,]
   
   train <- rbind(ceros.muestreado,unos.muestreado)
   test <- rbind(ceros.test,unos.test)
   
   colnames(train)[posi] <- "dependiente"
   colnames(test)[posi] <- "dependiente"
   
   
   
   
   reduccion = modprueba(data1(),data1org(),input$columns,input$radio1)
   
   s2 <- data1()
   
   
   PD <- predict(reduccion, newdata = s2, type = "response")
   return(cbind(PD))
   
   
   
   
 })
 
 
 
 data3 <- reactive({
   if(input$datasetr1){
     
     
     score1()
     
     
   }
   
   else {
     data <- pdPropias()
   }
 })
 
 #####################Se muestran los datos correspondientes a las pd de la cartera
 
 output$datatabler1<-renderDataTable({
   
   ca17 <- try(data3())
   
   
   if (class(ca17)=="try-error") {
     
     "Cargue datos"
     
   }else{ca17}
   
   
   
 },options = list(scrollX=T,scrollY=300))
 
 
 #### Sub seccion resultados
 
 
 ### exp
 
 datasetInputEXP <- reactive({
   # input$file1 will be NULL initially. After the user selects
   # and uploads a file, it will be a data frame with 'name',
   # 'size', 'type', and 'datapath' columns. The 'datapath'
   # column will contain the local filenames where the data can
   # be found.
   
   inFile <- input$file_dataEXP
   
   if (is.null(inFile))
     return(NULL)
   read.table(inFile$datapath, header = input$headerEXP,
              sep = input$sepEXP, quote = input$quoteEXP)
   
 })
 
 output$datatableEXP<-renderDataTable({
   datasetInputEXP()
 },options = list(scrollX=T,scrollY=300))
 
 
 
 ### probabilidades
 
 
 
 datasetInputPro <- reactive({
   # input$file1 will be NULL initially. After the user selects
   # and uploads a file, it will be a data frame with 'name',
   # 'size', 'type', and 'datapath' columns. The 'datapath'
   # column will contain the local filenames where the data can
   # be found.
   
   inFile <- input$file_dataPro
   
   if (is.null(inFile))
     return(NULL)
   read.table(inFile$datapath, header = input$headerPro,
              sep = input$sepPro, quote = input$quotePro)
   
 })
 
 datasetSelectPro <- reactive({
   datasetSelect <- proyec() 
 })
 
 
 Proba <- reactive({
   if(input$datasetPro){
     data <- datasetSelectPro()
     data <- as.data.frame(data)
     data <- data[3]
     }
   
   else {
     data <- datasetInputPro()
   }
 })
 
 ###Se muestran los datos
 
 
 output$datatablePro<-renderDataTable({
 Proba()
   
 },options = list(scrollX=T,scrollY=300))
 ### Baandas
 
 
 
 
 bandas <- function(uni,EAD,LGD){
   
   res <- ceiling((EAD*LGD)/uni)
   
   res1 <- as.data.frame(table(res))
   
   colnames(res1) <- c("Unidades de Pérdidas","Frequencia")
   
   return(res1)
   
   
 }  
 
 ######### perididas
 
 
 datasetInputPer <- reactive({
   # input$file1 will be NULL initially. After the user selects
   # and uploads a file, it will be a data frame with 'name',
   # 'size', 'type', and 'datapath' columns. The 'datapath'
   # column will contain the local filenames where the data can
   # be found.
   
   inFile <- input$file_dataPer
   
   if (is.null(inFile))
     return(NULL)
   read.table(inFile$datapath, header = input$headerPer,
              sep = input$sepPer, quote = input$quotePer)
   
 })
 
 
 
 ####Se muestran los datos
 
 
 output$PerdidaPropia<-renderDataTable({
   datasetInputPer()
 },options = list(scrollX=T,scrollY=300))
 
 
 
 #######
 
 perdidaconstr <- reactive({
   
   n = length(datasetInputEXP()[[1]])
   
   if (input$PerdiGene && !input$userFilePerd) {
     l <- as.data.frame(rep(input$PerEsp/100,n))
     colnames(l) <- c("Pérdida Dado el Incumplimiento")
     l
     
   }else if(!input$PerdiGene && input$userFilePerd){ 
     datasetInputPer()}
   
  
   
   
 })
 

 
 perclienv <- reactive({
   
   l<- cbind(datasetInputEXP(),Proba(),perdidaconstr())
   l1 <- l[1]*l[2]*l[3]
   colnames(l1) <- "Pérdida Esperada"
   l2 <- cbind(datasetInputEXP(),Proba(),perdidaconstr(),l1)
   
   l3 <- ceiling(l2[4]/input$uniper)
   
   names(l3) <- "Unidades de pérdida"
   
   
   cbind(l2,l3)
   

 }) 
 
 
 
 

 
 
 output$perclien <- renderDataTable({
   
   perclienv()
   
 },options = list(scrollX=T,scrollY=300)
 )

 
 
 
 
 probinc <- reactive({
   
   l <-  as.data.frame((-1*lfactorial(1:1000)))+ (as.data.frame(1:1000)*log(sum(Proba())))-(sum(Proba())*log(exp(1)))
   
   l1 <- exp(l) 
   
   colnames(l1) <- "Probabilidad de  Incumplimiento"
   l2 <- as.data.frame(1:1000)
   
   colnames(l2)  <- "Numero de Incumplimientos"
   return(cbind(l2,l1))
   
   
   
 })
 
 
 
 
 
 
 
 output$numincum <- renderDataTable({
   probinc()
 
 },options = list(scrollX=T,scrollY=300))
 
 
 
 ##############
 
 disn <- reactive({
   
   
   acum <- c()
   
   for (l in 1:1000) {
     acum[l] <- sum(probinc()[1:l,2]) 
   }
   
   
   acum <- as.data.frame(acum)
   num <- as.data.frame( 1:1000)
   final <- cbind(num,acum)
   colnames(final) <- c("Incumplimientos","Probabilidad")
   
   
   return(ggplotly(ggplot(final, aes(y=Probabilidad,x=Incumplimientos)) + geom_point()))
   
   
   
  
   
 })
 
 
 output$comparacion1 <- renderPlotly({
   
   disn()
   
 })
 
 
 #### perdidas por bandas agrupadas
 
 perd23v <- reactive({
   
   
   
   l <- perclienv()
   
   colnames(l) <- c("a","b","c","d","e")
   
   
   l1 <- ddply( l,~e,summarise,"Número Esperado de Incumplimientos"=sum(b))
   
   colnames(l1)[1] <- "Bandas de Exposición"
   
   po <- cbind(c(0),c(exp(-sum(l[2]))))
   colnames(po) <- c("Bandas de Exposición","Número Esperado de Incumplimientos")
   
   
   l2 <- rbind(po,l1)
   
   l3 <- l2[1]*l2[2]
   colnames(l3) <- "Pérdida Esperada por Banda"
   cbind(l2,l3)
   
   
 })
 
 output$Perd23 <- renderDataTable({
   
   perd23v()
   
 },options = list(scrollX=T,scrollY=300))
 
 #### Distribución de perdida de la cartera
 
 
 
 bandas1 <- reactive({
   
   
   
   l1 <- 1:100000
   l2 <- rep(0,100000) 
   bandas <- cbind(l1,l2)
   
   
   for(i in 2:dim(perd23v()[1])) {
     
     n = perd23v()[i,1]
     
     bandas[n,2] = perd23v()[i,3]
     
     
     
     
   }
   
   
   colnames(bandas) <- c("Bandas de Exposición","Pérdida Esperada por Banda")
   
   bandas <- rbind(perd23v()[1,c(1,3)],bandas)
   
   return(bandas)
  
   
 })
 
 
 
 
 percar <- reactive({
   
   prob <- NULL
   
    prob[1] <- exp(-sum(perclienv()[2]))
    
    for (i in 1:10000) {
      
     e <-  bandas1()[2:(i+1),2]
     p <- prob[1:i]
      
     p <- rev(p)
     
     prob[i+1] <- sum(e*p)/i
       
      
    }
    
    return(prob)
  
 })
 
 
 
 
 disn2 <- reactive({
   
   
   acum <- c()
   
   for (l in 1:10001) {
     acum[l] <- sum(percar()[1:l]) 
   }
   
   
   acum1 <- as.data.frame(acum)
   num <- as.data.frame( 0:10000)
   final <- cbind(num,acum1)
   colnames(final) <- c("Pérdida","Probabilidad")
   
   
   return(list(acum,ggplotly(ggplot(final, aes(y=Probabilidad,x=Pérdida)) + geom_point())))
   
   
   
   
   
 })
 
 
 output$comparacion2 <- renderPlotly({
   
   disn2()[[2]]
   
 })
### METRICAS DE RIESGO CREDITRISK +
 
 
 
 metr <- reactive({
   
   pro <- percar()
   n <- 0:(length(pro)-1)
   
   
   ## Perdida esperada
   pe <- sum(pro*n)*input$uniper
   
   
   ## var
   var <-  min(which(disn2()[[1]] > (as.numeric(input$conf)/100)))*input$uniper
   
   ### tvar
   
   c <- min(which(disn2()[[1]] > (as.numeric(input$conf)/100)))
   
   saltos <- diff(disn2()[[1]])
   
   v <- sum((saltos[c:length(saltos)]*c:10000))
   
   pw <- 1-sum(saltos[1:c])
   
   tvar <- v/pw*input$uniper
   return(list(pe,var,tvar))
   
 })
 
 ########### estress
 
 
 perclienvSt <- reactive({
   
   l<- cbind(datasetInputEXP(),Proba()*(1 + as.numeric(input$estres2)),perdidaconstr())
   l1 <- l[1]*l[2]*l[3]
   colnames(l1) <- "Pérdida Esperada"
   l2 <- cbind(datasetInputEXP(),Proba(),perdidaconstr(),l1)
   
   l3 <- ceiling(l2[4]/input$uniper)
   
   names(l3) <- "Unidades de pérdida"
   
   
   cbind(l2,l3)
   
   
 }) 
 
 
 perd23vSt <- reactive({
   
   
   
   l <- perclienvSt()
   
   colnames(l) <- c("a","b","c","d","e")
   
   
   l1 <- ddply( l,~e,summarise,"Número Esperado de Incumplimientos"=sum(b))
   
   colnames(l1)[1] <- "Bandas de Exposición"
   
   po <- cbind(c(0),c(exp(-sum(l[2]))))
   colnames(po) <- c("Bandas de Exposición","Número Esperado de Incumplimientos")
   
   
   l2 <- rbind(po,l1)
   
   l3 <- l2[1]*l2[2]
   colnames(l3) <- "Pérdida Esperada por Banda"
   cbind(l2,l3)
   
   
 })
 
 
 bandas1St <- reactive({
   
   
   
   l1 <- 1:100000
   l2 <- rep(0,100000) 
   bandas <- cbind(l1,l2)
   
   
   for(i in 2:dim(perd23vSt()[1])) {
     
     n = perd23vSt()[i,1]
     
     bandas[n,2] = perd23vSt()[i,3]
     
     
     
     
   }
   
   
   colnames(bandas) <- c("Bandas de Exposición","Pérdida Esperada por Banda")
   
   bandas <- rbind(perd23vSt()[1,c(1,3)],bandas)
   
   return(bandas)
   
   
 })
 
 percarSt <- reactive({
   
   prob <- NULL
   
   prob[1] <- exp(-sum(perclienvSt()[2]))
   
   for (i in 1:10000) {
     
     e <-  bandas1St()[2:(i+1),2]
     p <- prob[1:i]
     
     p <- rev(p)
     
     prob[i+1] <- sum(e*p)/i
     
     
   }
   
   return(prob)
   
 })
 
 
 disn2St <- reactive({
   
   
   acum <- c()
   
   for (l in 1:10001) {
     acum[l] <- sum(percarSt()[1:l]) 
   }
   
   
   acum1 <- as.data.frame(acum)
   num <- as.data.frame( 0:10000)
   final <- cbind(num,acum1)
   colnames(final) <- c("Pérdida","Probabilidad")
   
   
   return(acum)
   
 })
 
 Stress <-reactive({
   
   
   
   
   pro <- percar()
   n <- 0:(length(pro)-1)
   
   
 
  
   varS <-  min(which(disn2St() > (as.numeric(input$conf)/100)))*input$uniper
   
   
   return(varS)
   
   
    
 })
 
 output$Stress <- renderText({
   
   ca18 <- try( Stress())
   
   
   if (class(ca18)=="try-error") {
     
     "Cargue datos"
     
   }else{ca18}
   
   
 })
 
 
 
 
 
 #################### La funcion CreditTR calcula las metricas de riesgo
 
 
 
 
 
 CrediTR <- reactive({
   
   
   # Primero necesitamos las espocisiones al default
   
   s1 <- data1()
   
   
   
   
   ###supondremos que son activos sin lineas extra 
   ### en este caso la exposicion coincide con el saldo
   
   ##exposicion de la cartera
   #View(mydata)
   
   
   EAD <- data1org()[,"Credit.Amount"]
   
   ###supondremos que la perdida dado el default es la misma para toda  la cartera
   ### la institucion puede ajustar a un cliente en particular una perdida diferente
   
   LGD <- (100-input$uni)/100
   
   
   DP= data3()
   
   ####Calculamos ahora la perdida esperada
   
   EL <- EAD*LGD*DP[,"PD"]
   
   
   ### Se calcula la expocicion que se espera pérder en caso de default
   
   Ei <- EAD*LGD
   
   #### Se escoge una unidad de perdida
   
   E <- input$uniper
   
   
   
   
   ###### se calculan las unidades de perdida
   
   v <- Ei/E
   
   e <- EL/E
   
   ###se calcula el paramtro de poisson
   ### correspondiente a la posibilidad de incumplimiento
   
   lambda <- -log(1-DP[,"PD"])
   
   
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
   
   
   saltos <- diff(acum)
   pe <- (saltos*1:9999)*E
   pe1 <- sum(pe)
   
   #####Var
   var <-  min(which(acum > (as.numeric(input$conf)/100)))*E
   
   
   
   #### TVAR
   
   
   
   
   
   c <- min(which(acum > (as.numeric(input$conf)/100)))
   
   v <- sum((saltos[c:length(saltos)]*c:9999))
   
   pw <- 1-sum(saltos[1:c])
   
   tvar <- v/pw*E
   
   
   
   return(c(pe1,var,tvar))
   
 })
 
 ################# Se muestra la perdida esperada
 
 output$pe <- renderText({
   
   
   ca <- try(metr()[[1]])
   if (class(ca)=="try-error") {
     
     "Cargue datos y seleccione parametros"
   }else{ca}
   
   
   
 })
 
 
 #################Se muestra el var
 
 output$var <- renderText({
   
   
   ca1 <- try(metr()[[2]])
   if (class(ca1)=="try-error") {
     
     "Cargue datos y seleccione parametros"
   }else{ca1}
   
 
 })

 
 ########## Se muestra el Tvar
 
 output$tvar <- renderText({
   
   
   ca2 <- try(metr()[[3]])
   if (class(ca2)=="try-error") {
     
     "Cargue datos y seleccione parametros"
   }else{ca2}
   
   
   
   
 })
 
 
 ########## Aqui se realiza el reporte
 
 output$reporte1 <- downloadHandler(
   
   filename = "reporte1.pdf",
   content = function(file){
     tempReport <- file.path(tempdir(),"reporte1.Rmd")
     file.copy("reporte1.Rmd", tempReport, overwrite = TRUE)
     params <- list(titulo2=c(CrediTR()[2]),titulo3=c(CrediTR()[1]),titulo4=c(CrediTR()[3]),
                    titulo5=c(modprueba(data1(),data1org(),input$columns,input$radio1)) ,titulo6=calroc(data1(),data1org(),input$columns,modprueba(data1(),data1org(),input$columns,input$radio1)),titulo7=input$radio1, titulo8=input$uniper, titulo9=input$uni,
                    titulo10 = calaccur(), titulo11 = input$significancia, raroc = raroc1333(),
                    rorac = rorac1(), rarorac = rarorac1(), morosidad = (as.numeric(datasetInputindices()[1,2])/as.numeric(datasetInputindices()[2,2])), cobertura=(as.numeric(datasetInputindices()[1,2])/as.numeric(datasetInputindices()[3,2])),
                    rar = rar1()
                              )
     
     
     
     
     rmarkdown::render(tempReport,output_file = file,params = params, envir = new.env(parent = globalenv()))
   }
 )
 
 
 
 
 ####### Seccion Stress testing
 
 
 
 StressT <- reactive({
   
   
   
   # Primero necesitamos las espocisiones al default
   
   s1 <- data1org()
   
   
   
   
   ###supondremos que son activos sin lineas extra 
   ### en este caso la exposicion coincide con el saldo
   
   ##exposicion de la cartera
   #View(mydata)
   
   
   EAD <- s1[,"Credit.Amount"]
   
   ###supondremos que la perdida dado el default es la misma para toda  la cartera
   ### la institucion puede ajustar a un cliente en particular una perdida diferente
   
   LGD <- (100-input$uni)/100
   
   
   DP= data3()
   
   for (i in 1:length(DP[,"PD"])) {
     
     if (DP[i,"PD"]<0.4) {
       DP[i,"PD"]<- DP[i,"PD"] + as.numeric(input$estres2)
     }
     
   }
   
   ####Calculamos ahora la perdida esperada
   
   EL <- EAD*LGD*DP[,"PD"]
   
   
   ### Se calcula la expocicion que se espera pérder en caso de default
   
   Ei <- EAD*LGD
   
   #### Se escoge una unidad de perdida
   
   E <- input$uniper
   
   
   
   
   ###### se calculan las unidades de perdida
   
   v <- Ei/E
   
   e <- EL/E
   
   ###se calcula el paramtro de poisson
   ### correspondiente a la posibilidad de incumplimiento
   
   lambda <- -log(1-DP[,"PD"])
   
   
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
   var <-  min(which(acum > (as.numeric(input$conf)/100)))*E
   return(var)
   
 })
 
 

 
 ####### CreditMetrics#######################
 
 
 ### Seccion Creditos  
 
 ### Se cargan y se muestran los datos de la manera usual
 
 datasetSelect0 <- reactive({
   datasetSelect0 <- creditos
 })
 
 
 datasetInput0 <- reactive({
   
   inFiler <- input$file_datacrm0
   
   if (is.null(inFiler))
     return(NULL)
   read.table(inFiler$datapath, header = input$headecrm0,
              sep = input$sepcrm0, quote = input$quotecrm0)
   
 })
 
 
 data6 <- reactive({
   if(input$dataset0){
     data <- datasetSelect0()}
   
   else {
     data <- datasetInput0()
   }
 })
 

 output$datatable0<-renderDataTable({
   data6()
 },options = list(scrollX=T,scrollY=300))
 
 
 
 
 
## Seccion matriz de transicion
 
 
 ##### Subseccion calculo de la matriz de transicion+
 
 ##### Se cargan y se muestran los datos relacionados para el calculo de la matriz de transicion
 
 
 
 
 datasetSelectMT <- reactive({
   datasetSelect <- transic
 })
 
 
 
 
 
 datasetInputMT <- reactive({
   
   inFile <- input$file_dataMT
   
   if (is.null(inFile))
     return(NULL)
   read.table(inFile$datapath, header = input$headerMT,
              sep = input$sepMT, quote = input$quoteMT)
   
 })
 
 
 
 data10 <- reactive({
   if(input$datasetMT){
     data <- datasetSelectMT()}
   
   else {
     data <- datasetInputMT()
   }
 })
 output$datatableMT<-renderDataTable({
   data10()
 },options = list(scrollX=T,scrollY=300))
 
 
 
 ##### Funcion que calcula la matriz de transicion
 
 MTR <- function(migra){
   
   MT <- as.data.frame.matrix(table(migra))
   
   for (i in 1:ncol(MT)) {
     
     sum1 <- sum(MT[,i])
     MT[,i] <- MT[,i]/sum1
     
   }
   
   MT <- round(MT*100,1)
   
   MT <- cbind(colnames(MT),MT)
   colnames(MT)[1]<-"Inicio/fin"
   return(MT)
  
 }
 
 
### Se muestra la matris de transicion calculada
 
 output$datatableMTR<-renderDataTable({
   
   MTR(data10())
   
 },options = list(scrollX=T,scrollY=300))
 
 
 
 #### subseccion seleccion de la matriz de transicion
 
 
 MatrizPropias <- reactive({
   
   inFiler <- input$file_datacrm
   
   if (is.null(inFiler))
     return(NULL)
   read.table(inFiler$datapath, header = input$headecrm,
              sep = input$sepcrm, quote = input$quotecrm)
   
 })
 
 
 #### data4 contiene ka matriz de seleccion calculada
 
 data4 <- reactive({
   if(input$datasetcrm){
     
     
     MTR(data10())
     
     
   }
   
   else {
     data <- MatrizPropias()
   }
 })
 
 ###SE muestra a matriz de transicion
 
 output$datatablecrm<-renderDataTable({
   data4()
 },options = list(scrollX=T,scrollY=300))
 
 
 
 
 ### Seccion perdida por clase
 
 ##### Subseccion calculo de las pérdidas esperadas
 
 ### Se cargan los datos
 
 datasetSelectC <- reactive({
   datasetSelect <- clases1
 })
 
 
 
 
 
 datasetInputC <- reactive({
   
   inFile <- input$file_dataC
   
   if (is.null(inFile))
     return(NULL)
   read.table(inFile$datapath, header = input$headerMT,
              sep = input$sepMT, quote = input$quoteMT)
   
 })
 
 
 
 data11 <- reactive({
   if(input$datasetC){
     data <- datasetSelectC()}
   
   else {
     data <- datasetInputC()
   }
 })
 
 ### Se muestran los datos
 
 output$datatableC<-renderDataTable({
   data11()
 },options = list(scrollX=T,scrollY=300))
 
 #####
 
 
 
 CR <- function(histo){
   

   
   N <- NULL
   
   
   clases <- levels(histo[,1])
   
   for (i in 1:length(clases)) {
     
     s <- which(histo[,1]==clases[i])
     
     s1 <- histo[,2]
     
     N[i] <- mean(s1[s])
     
   }
   
   N <- paste(round(N,2),"%",sep ="" ) 
   
   
   result <- data.frame(N,clases)
   
   colnames(result) <- c("Perdida" , "Calif")
   
   return(result)
   
   
   
 }
 
 
 
 
 
 #### Subseccion perdida esperada
 
  

 datasetInputcrm1 <- reactive({
   datasetInputcrm1 <-CR(data11())
 })
 
 
 data5 <- reactive({
  
     datasetInputcrm1() 
   
  
 })
 
 
 
 bostCL <- function(cla, n ,tam){
   
   clases <- names(table(cla["clases"]))
   
   medias <- NULL
   desviacion <- NULL
   
   
   
   for (i in 1:length(clases)) {
     
     pos <- which(cla[,1]==clases[i])
     
     filtro <- cla[pos,2]
     
     mediabost <- NULL
     for ( j  in 1:n) {
       muestra <- sample(filtro,ceiling((tam/100)*length(filtro)),replace = T)
       mediabost[j] <- mean(muestra)
     }
     
     medias[i] <- mean(mediabost)
     desviacion[i] <- sd(mediabost)
     
     
   }
   
   
   return(list(medias,desviacion,clases))
   
 }
 
 clasescal <- reactive({
   
   bostCL(data11(), as.integer(input$bootC) , as.integer(input$bootTC)) 
   
   
 })
 
 clasecrm <- reactive({
   
   l <- as.data.frame(clasescal()[[1]])
   k <- as.data.frame(clasescal()[[2]])
   
   j <- k*qnorm((1-(as.numeric(input$boot2312)/100 ))/2)/sqrt(as.numeric(input$bootC))
   
   
   iz <- l+j
   de <- l-j
   final <- cbind(l,iz,de)
   
   colnames(final) <- c("Pérdida Esperada","Mínimo Esperado","Máximo Esperado")
   
   d <- cbind(clasescal()[[3]],round(final,2))
   colnames(d)[1] <- "Clase" 
   d
 })
 
 output$datatablecrm2 <- renderDataTable({
   
   clasecrm()
   
 })
 
### Se muestra la perdida por clases a usar para la metodologuia
 
 output$datatablecrm1<-renderDataTable({
   
   ca22 <- try(data5())
   
   
   if (class(ca22)=="try-error") {
     
     "Cargue datos"
     
   }else{ca22}
   
   
   
   
   
 })
  
  ############# Seccion simulacion y resultados
 
 
 #### calvar calcula las metricas de riesgo
 
 
 calvar <-eventReactive(input$goButtonSim,{
   
   
     
   
     ## Matriz de transicion
     MT <- data4()
     ## Perdida por clase
     
     RP <- clasecrm()
     # 
     #clasificaciones  
    clasi <- colnames(MT)[2:ncol(MT)]
     # 
     # 
     # 
      creditos <- data6()
     # 
     # 
     # 
      
      
      muestras <- list()
      
      
      for (i in 1:length(clasi)) {
        
        prob = as.numeric(MT[,i+1])
        
        sim <- NULL
        
        
        for (j in 1:length(clasi)) {
          
          sim <- c(sim,rep(clasi[j],round(10*prob[j],0)))
          
        }
        
        muestras[[i]] <- sim
      }
      
      
      
      NSim <- as.numeric(input$simcrm)
     # 
      Total <- NULL
      for (j in 1:NSim) {
        
      
      
      acum <- NULL
      for (i in 1:length(clasi)) {
        
        l <- subset(creditos,calif==clasi[i])
        trans <- sample(muestras[[i]],nrow(l))
        per <- NULL
        
        for (k in 1:length(trans)) {
          
          per[k] <- l[k,"creditos"]*as.numeric(RP[which(RP[,"Clase"]==trans[k]),"Pérdida Esperada"])/100
          
        }
        
        acum[i] <- sum(per)
        
        
      }
      
      Total[j] <- sum(acum)
      
      }
      
      peresp <- mean(Total)
      
      var <- mean(Total)+(sd(Total)*qnorm(as.numeric(input$conf1)/100))
      
      tvar <- mean(Total)+((sd(Total)*dnorm(qnorm(as.numeric(input$conf1)/100)))/(1-(as.numeric(input$conf1)/100)))
      
      strescr <- (mean(Total)+(mean(Total)*as.numeric(input$stress3)))+((sd(Total)+sd(Total)*as.numeric(input$stress3))*qnorm(as.numeric(input$conf1)/100))
       
      total1 <- as.data.frame(Total)
      num <- 1:input$simcrm
      
      total1 <- cbind(total1,num)
      colnames(total1)[1] <- "Perdidas"
      p7 <- ggplot(total1, aes(x = Perdidas)) +
        geom_histogram()+labs(x = "Pérdida",y = "Frecuencia")
      
     
     return(list(peresp,var,tvar,strescr,p7))
  
   
 })
 

 
 
 
 output$credime <- renderPlotly({
   
   
   
   ca7 <- try(calvar()[[5]])
   
   
   if (class(ca7)=="try-error") {
     
     df <- data.frame()
     ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
     
   }else{ca7}
   
   
   
 })
 
 
  ##### Se muestra la perdida esperada
 
 output$pe122 <- renderText({
   ca4 <- try(calvar()[[1]])
   if (class(ca4)=="try-error") {

     "Cargue datos y seleccione parametros"
   }else{ca4} })

   
   
   
 
 
 #### Se muestra el var
 
 
 
 output$var122 <- renderText({

   ca5 <- try(calvar()[[2]])
   if (class(ca5)=="try-error") {

     "Cargue datos y seleccione parametros"
   }else{ca5}

 })

 
 
 
 #### Se muestra el Tvar

  
 output$tvar122 <- renderText({

   ca6 <- try(calvar()[[3]])
   if (class(ca6)=="try-error") {

     "Cargue datos y seleccione parametros"
   }else{ca6}

 })
  
 
 
 
 
######## El reporte de credimetrics
 
 output$reporte2 <- downloadHandler(
   
   filename = "reporte2.pdf",
   content = function(file){
     tempReport <- file.path(tempdir(),"reporte2.Rmd")
     file.copy("reporte2.Rmd", tempReport, overwrite = TRUE)
     params <- list(vari1 =data4(),vari2=data5(),vari3=calvar(),vari4 = input$simcrm, raroc = raroc1333(),
                    rorac = rorac1(), rarorac = rarorac1(), morosidad = (as.numeric(datasetInputindices()[1,2])/as.numeric(datasetInputindices()[2,2])), cobertura=(as.numeric(datasetInputindices()[1,2])/as.numeric(datasetInputindices()[3,2])),
                    rar = rar1())
     
     
     
     
     rmarkdown::render(tempReport,output_file = file,params = params, envir = new.env(parent = globalenv()))
   }
 )
 
  
  
### Seccion Stresting
 
 ##### Simplemente se llama a clavar
 output$Stres45 <- renderText({


   ca23 <- try(calvar()[[4]])


   if (class(ca23)=="try-error") {

     "Cargue datos"

   }else{ca23}

 })
 #  
  
  
################### Indicadores contables#################
 
  
  
  calraroc <- reactive({
    varr <- NULL
    per <- NULL
    ing <- NULL
    
      
    if (input$meto==2) {
        varr = as.numeric(calvar()[[1]])
        per <- as.numeric(calvar()[[2]])
        ing <- 138519.4
        
        raroc <- (ing-per)/varr
    }else{
      
      varr <-  CrediTR()[2]
      per <- CrediTR()[1]
      ing <- 1385190.4
      raroc <- (ing-per)/varr
      }
      
      
    
    
    return(raroc)
  })
  
  
 
 
  rar1 <- reactive({
    
    per <- NULL
    
    
   valor <-  try(
    if (input$meto==2) {
      
      per <- as.numeric(calvar()[[2]])
      
      
      rar <- as.numeric(datasetInputindices()[4,2])-per
    }else{
      
      
      per <- CrediTR()[1]
      
      rar <- as.numeric(datasetInputindices()[4,2])-per
    })
    
    
   
   if (class(valor)=="try-error") {
     
     retorno <- " "
     
   }else{retorno <- rar }
    
    
    return(retorno)
  })
  
  
  rorac1 <- reactive({
    
    valor <- try(
    if (input$meto==2) {
      
      
      a <- as.numeric(datasetInputindices()[4,2])
      
      rac <- calvar()[[4]]
      
      rorac<- a/rac
      
    }else{
      
      a <- as.numeric(datasetInputindices()[4,2])
      
      rac <- CrediTR()[3]
      
      rorac<- a/rac
      
    })    
    
    if (class(valor)=="try-error") {
      
      retorno <- " "
      
    }else{retorno <- rorac }
    
    
    return(retorno)
  })
  
  
  rarorac1 <- reactive({
    
    
    
    
    
      
      rar <- rar1()
      valor =try(
    
    if (input$meto==2) {
      
      
      a <- as.numeric(datasetInputindices()[4,2])
      
      rac <- calvar()[[4]]
      
      
      
      rarora <- rar/rac
      
    }else{
      
      a <- as.numeric(datasetInputindices()[4,2])
      
      rac <-  CrediTR()[3]
      
      
      rarora <- rar/rac
      
    }    
    
    )
    
    
      
      if (class(valor)=="try-error") {
        
        retorno <- " "
        
      }else{retorno <- rarora }
      
    return(retorno)
  })
  
  
  
  raroc1333 <- reactive({
    
    rar <- rar1()
    
    
    valor <- try(
    
    if (input$meto==2) {
      
      a <- as.numeric(datasetInputindices()[4,2])
      
      var <- calvar()[[1]]
      
      raroc <- rar/var
      
    }else{
      
      
      var <-  CrediTR()[2]
      
      
      raroc <- rar/var
    }    )
    
    if (class(valor)=="try-error") {
      
      retorno <- " "
      
    }else{retorno <- raroc }
    
    
    
    return(retorno)
  })
  
  output$Raroc1 <- renderText ({
    
    
    
  calraroc()
    
  })
  
  
  
  
  output$rar <- renderText ({
    
    
    
    rar1()
    
  })
  output$roracc <- renderText ({
    
    
    
    rorac1()
    
  })
  
  
  output$raroracc <- renderText ({
    
    
    
    rarorac1()
    
  })
  
  output$raro <- renderText ({
 
    
    raroc1333()
    
  })
  
  
  
  
  
  
 
  
  
  
  
  datasetInputindices <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$indices
    
    if (is.null(inFile))
      return(NULL)
    read.table(inFile$datapath, header = input$headerind,
               sep = input$sepind, quote = input$quoteind)
    
  })
  
  output$datatableind<-renderDataTable({
    datasetInputindices()
  },options = list(scrollX=T,scrollY=300))
  
  output$morosidad <- renderText({ 
    a <- as.numeric(datasetInputindices()[1,2])
    b <- as.numeric(datasetInputindices()[2,2])
    a/b
  })
  
  
  output$cobertura <- renderText({ 
    c <- as.numeric(datasetInputindices()[1,2])
    d <- as.numeric(datasetInputindices()[3,2])
    c/d
  })
  
  
  
  
  

  
})