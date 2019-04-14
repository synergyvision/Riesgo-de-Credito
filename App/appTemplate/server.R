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
    
    ca7 <- try(ggplotly(grafica(data1org(),input$columns,input$columns1)))
    
    
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
       pval[i]<-pr$p.value
       
       
     }
     
     
     pval <- t(pval)
     
     
     vd <- nomb[which(pval > 0.05)]
     
     
     j <- colnames(datos)
     
     
     
     inf <- data.frame(pval)
     colnames(inf) <- nomb
     return(inf)
   }
  ## se calculan los pvalores
   
  pvalor <- reactive({pval(data1org())})
   
   
  ## seccion que muestra los p-valores de las variables cualitativas
   
   
  output$datatablecu <- renderDataTable({
    
    
    ca10 <- try(pvalor())
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
    
    nomb <- colnames(D1)
    
    
    for (i in 2:length(nomb)) {
      
      df1 <- D1[,c(1,i)]
      
      d0 <- subset(df1, Creditability==0)
      d1 <- subset(df1, Creditability==1)
      
      p1 <- d0[[2]]
      p2 <- d1[[2]]
      
      
      w <-  ks.test(p1,p2)
      
      pval[i] <- w$p.value
      
    }
    
    pval <- t(pval)
    
    
    inf <- data.frame(pval)
    colnames(inf)<-nomb
    
    return(inf)
    
    
  }
  
  
  #### En la variable data1. se eliminan las variables cualitativas que tengan 
  #### un nivel de significancia mayor al del deseado por el usuarios el nivel esta en la variable input$significancia
  
  data1. <- reactive({
    
    
    datos <- data1org()
    
    pvalores <- pvalor()
    
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
    ca11 <- try(pvalor1())
    if (class(ca11)=="try-error") {
      
      "Cargue datos y seleccione parametros"
    }else{ca11}
    
  })
 
   
  
  ### Se eliminan las variables cuantitativas con un nivel de significancia mayor al del requerido por el usuario
  
  

  
 data1 <- reactive({
   
   datos <- data1.()
   
   pvalores <- pvalor1()
   
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
   datasetSelectrl <- lgd
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
   
   lgd <-data7()
   
   nc <- dim(lgd)[2]
   
   
   lgdp <- NULL
   
   
   
   
   for (i in 1:nc) {
     
     lgdp[i] <- median(lgd[,i])
     
   }
   
   lgdp <- as.data.frame(lgdp)
   
   lgdp[2] <- 1:24
   
   
   
   colnames(lgdp) <- c("Porcentajes","Periodos")
   
   
   p <- plot_ly(lgdp, x = ~Periodos, y = ~Porcentajes, name = 'trace 0', type = 'scatter', mode = 'lines') 
   retornar <- list(p,lgdp)
   
   return(retornar)
   
 })
 
 ### aqui se muestra la grafica.
 
 output$curvalgd <- renderPlotly({
   
   ca13 <- try(lgd1()[[1]])
   
   
   if (class(ca13)=="try-error") {
     
     df <- data.frame()
     ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
     
   }else{ca13}
 
   
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
 
 
 

 ############### Subseccion Score de la cartera de credito
 
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
 

 
 ######## subcesion Proyeccion a nuevos clientes
 
 
 
 #### Se cargan los datos de la manera usual
 
 
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
 
 
 ### Se muestran los datos
 
 
 output$datatabler<-renderDataTable({
   data2()
 })
 
 
  
  
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
 
 #################### La funcion CreditTR calcula las metricas de riesgo
 
 CrediTR <- reactive({
   
   
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
   
   
   ca <- try(CrediTR()[1])
   if (class(ca)=="try-error") {
     
     "Cargue datos y seleccione parametros"
   }else{ca}
   
   
   
 })
 
 
 #################Se muestra el var
 
 output$var <- renderText({
   
   
   ca1 <- try(CrediTR()[2])
   if (class(ca1)=="try-error") {
     
     "Cargue datos y seleccione parametros"
   }else{ca1}
   
   
   
   
   
 })
 
 

 
 ########## Se muestra el Tvar
 
 output$tvar <- renderText({
   
   
   ca2 <- try(CrediTR()[3])
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
     params <- list(titulo =c(posicion()),titulo2=c(CrediTR()[2]),titulo3=c(CrediTR()[1]),titulo4=c(CrediTR()[3]),
                    titulo5=c(modprueba(data1(),data1org(),input$columns,input$radio1)) ,titulo6=calroc(data1(),data1org(),input$columns,modprueba(data1(),data1org(),input$columns,input$radio1)),titulo7=input$radio1, titulo8=input$uniper, titulo9=input$uni,
                    titulo10 = calaccur(), titulo11 = input$significancia)
     
     
     
     
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
 
 
 output$Stress <- renderText({
   
   ca18 <- try(StressT())
   
   
   if (class(ca18)=="try-error") {
     
     "Cargue datos"
     
   }else{ca18}
   
   
   
   
 })
 
 
 
 ################
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
  
  
  
  
  
  
  output$variables1 <- renderText({
    
    s1 <- data1org()
    
    
    tamano <- 1:length(names(s1))
    
    paste(tamano,names(s1),sep = "-") 
  })
  
  output$varia23 <- renderText({
    
    s1 <- data1org()
    
    
    tamano <- 1:length(names(s1))
    
    paste(tamano,names(s1),sep = "-") 
  })
  

  
  posicion <- reactive({
    
    s1 <- data1()
    # 
    
    nombres <- colnames(data1org())
    
    nombre <- input$columns
    
   which(nombres == nombre)
    
  })
  
  
  
  output$datatable<-renderDataTable({
    data1org()
  },options = list(scrollX=T,scrollY=300))
  
 
  
  
 
  
  
  
  
  
  
  
  
  
  
  
 
  
  ###Datos
  
 
  

  
  
  
  
  
  
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 
  
  
  
  
  
  

  
  
  
  
  MatrizPropias <- reactive({
    
    inFiler <- input$file_datacrm
    
    if (is.null(inFiler))
      return(NULL)
    read.table(inFiler$datapath, header = input$headecrm,
               sep = input$sepcrm, quote = input$quotecrm)
    
  })
  
  MTR <- reactive({
    
    migra <- data10()
    clases <- levels(migra[,1])
    
    
    periodos <- length(migra)/2
    
    n <- NULL
    
    
    
    for (k in 1:periodos) {
      
      
      
      for (j in 1:length(clases)) {
        
        
        orig <- length(which(migra[,(2*k)-1]==clases[j]))
        s <- migra[,2*k]
        s1<- s[which(migra[,(2*k)-1]==clases[j])]
        
        
        for (i in 1:length(clases)) {
          
          
          final <-length(which(s1==clases[i]))
          
          
          n[i+(5*(j-1))+(25*(k-1))] <- 100*final/orig
          
          
        }
        
        
      }
      
      
      
    }
    
    
    
    
    
    o <- matrix(numeric(25),nrow = 5)
    
    
    
    for (b in 0:9) {
      
      h <- matrix(n[(1+(25*b)):(25+(25*b))],5, byrow = T)
      o <- o + h
      
    }
    
    Matrix <- o / 10
    colnames(Matrix) <- clases
    MT <- round(Matrix)
    
    
    return(as.data.frame(MT))
    
    
    
  })
  
  
  
  output$datatableMTR<-renderDataTable({
    
    MTR()
    
  },options = list(scrollX=T,scrollY=300))
  
  
  
  data4 <- reactive({
    if(input$datasetcrm){
      
      
      MTR()
      
      
    }
    
    else {
      data <- MatrizPropias()
    }
  })
  
  ###Datos
  
  output$datatablecrm<-renderDataTable({
    data4()
  },options = list(scrollX=T,scrollY=300))
  
  
  
  
  clasesPropias <- reactive({
    
    inFiler <- input$file_datacrm1
    
    if (is.null(inFiler))
      return(NULL)
    read.table(inFiler$datapath, header = input$headecrm1,
               sep = input$sepcrm1, quote = input$quotecrm1)
    
  })
  
  
  datasetInputcrm1 <- reactive({
    datasetInputcrm1 <-CR()
  })
  
  
  data5 <- reactive({
    if(input$datasetcrm1){
      data <-datasetInputcrm1() }
    
    else {
      data <- clasesPropias()
    }
  })
  
  
  
  
  
  
  
  ###Datos
  
  output$datatablecrm1<-renderDataTable({
   
    ca22 <- try(data5())
    
    
    if (class(ca22)=="try-error") {
      
      "Cargue datos"
      
    }else{ca22}
    
    
 
    
    
    })
  
  
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
  
  ###Datos
  
  output$datatable0<-renderDataTable({
    data6()
  },options = list(scrollX=T,scrollY=300))
  
  
  calvar <- reactive({
    
    withProgress(message="simulando", value = 0,{
      MT <- data4()
      
      
      clasi <- colnames(MT)
      
      
      RP <- data5()
      RP <- RP[,"Perdida"]
      
      creditos1 <- data6()
      
      
      
      NSim <- as.numeric(input$simcrm)
      
      M <- NULL
      for (j in 1:NSim) {
        
        
        N <- NULL 
        for (i in 1:length(clasi)) {
          l <- subset(creditos1,calif==clasi[i]) 
          g <- dice.roll(faces=length(clasi), dice=length(l[,"creditos"]), rolls=1, weights=as.numeric(MT[i,]/100))
          
          N[i] <- sum((l[,"creditos"])*RP[as.numeric(g$results[,1:length(l[,"creditos"])])])
        }
        
        M[j] <- sum(N)
        
      }
      
      var <- mean(M)+(sd(M)*qnorm(as.numeric(input$conf1)/100))
      
      tvar <- mean(M)+((sd(M)*dnorm(qnorm(as.numeric(input$conf1)/100)))/(1-(as.numeric(input$conf1)/100)))
      
      strescr <- (mean(M)+(mean(M)*as.numeric(input$stress3)))+((sd(M)+sd(M)*as.numeric(input$stress3))*qnorm(as.numeric(input$conf1)/100))
      
      return(list(var,mean(M),M,tvar, strescr))
    })
    
  })
  
  output$pe122 <- renderText({
    ca4 <- try(calvar()[[2]])
    if (class(ca4)=="try-error") {
      
      "Cargue datos y seleccione parametros"
    }else{ca4}
    
    
    
    
  })  
  
  output$var122 <- renderText({
    
    ca5 <- try(calvar()[[1]])
    if (class(ca5)=="try-error") {
      
      "Cargue datos y seleccione parametros"
    }else{ca5}
    
    
    
    
    
  })  
  
  output$tvar122 <- renderText({
    
    ca6 <- try(calvar()[[4]])
    if (class(ca6)=="try-error") {
      
      "Cargue datos y seleccione parametros"
    }else{ca6}
    
    
    
    
    
  })  
  
  
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
  output$datatableC<-renderDataTable({
    data11()
  },options = list(scrollX=T,scrollY=300))
  
  
  
  
  CR <- reactive({
    
    histo <- data11()
    
    N <- NULL
    
    
    clases <- levels(histo[,1])
    
    for (i in 1:length(clases)) {
      
      s <- which(histo[,1]==clases[i])
      
      s1 <- histo[,2]
      
      N[i] <- mean(s1[s])
      
    }
    
    N <- N /100
    
    
    result <- data.frame(N,clases)
    
    colnames(result) <- c("Perdida" , "Calif")
    
    return(result)
    
    
    
  })
  
  
  
  output$datatableCR<-renderDataTable({
    
    ca20 <- try(CR())
    if (class(ca20)=="try-error") {
      
      "Cargue datos"
    }else{ca20}
    
    
    
  },options = list(scrollX=T,scrollY=300))
  
  
  calraroc <- reactive({
    varr <- NULL
    per <- NULL
    ing <- NULL
    
      
    if (input$meto==2) {
        varr = as.numeric(calvar()[1])
        per <- as.numeric(calvar()[2])
        ing <- 138519.4
        
        raroc <- (ing-per)/varr
    }else{
      
      varr <-  calvar1()
      per <- calpe()
      ing <- 1385190.4
      raroc <- (ing-per)/varr
      }
      
      
    
    
    return(raroc)
  })
  
  
  rar1 <- reactive({
    
    per <- NULL
    
    
    
    if (input$meto==2) {
      
      per <- as.numeric(calvar()[2])
      
      
      rar <- as.numeric(datasetInputindices()[4,2])-per
    }else{
      
      
      per <- calpe()
      
      rar <- as.numeric(datasetInputindices()[4,2])-per
    }
    
    
    
    
    return(rar)
  })
  
  rorac1 <- reactive({
    
    
    if (input$meto==2) {
      
      
      a <- as.numeric(datasetInputindices()[4,2])
      
      rac <- calvar()[[4]]
      
      rorac<- a/rac
      
    }else{
      
      a <- as.numeric(datasetInputindices()[4,2])
      
      rac <- caltvar()
      
      rorac<- a/rac
      
    }    
    
    
    
    
    return(rorac)
  })
  
  
  rarorac1 <- reactive({
    
    rar <- rar1()
    
    
    if (input$meto==2) {
      
      
      a <- as.numeric(datasetInputindices()[4,2])
      
      rac <- calvar()[[4]]
      
      
      
      rarora <- rar/rac
      
    }else{
      
      a <- as.numeric(datasetInputindices()[4,2])
      
      rac <- caltvar()
      
      
      rarora <- rar/rac
      
    }    
    
    
    
    
    return(rarora)
  })
  
  
  
  raroc1333 <- reactive({
    
    rar <- rar1()
    
    
    if (input$meto==2) {
      
      a <- as.numeric(datasetInputindices()[4,2])
      
      var <- calvar()[[1]]
      
      raroc <- rar/var
      
    }else{
      
      
      var <- calvar1()
      
      
      raroc <- rar/var
    }    
    
    
    
    
    return(raroc)
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
  
  
  
  
  
  
  output$Stres45 <- renderText({
  
    
    ca23 <- try(calvar()[[5]])
    
    
    if (class(ca23)=="try-error") {
      
      "Cargue datos"
      
    }else{ca23}
    
      
    
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
  
  
  
  
  
  output$reporte2 <- downloadHandler(
    
    filename = "reporte2.pdf",
    content = function(file){
      tempReport <- file.path(tempdir(),"reporte2.Rmd")
      file.copy("reporte2.Rmd", tempReport, overwrite = TRUE)
      params <- list(vari1 =data4(),vari2=data5(),vari3=calvar(),vari4 = input$simcrm)
      
      
      
      
      rmarkdown::render(tempReport,output_file = file,params = params, envir = new.env(parent = globalenv()))
    }
  )
  
})