shinyServer(function(input, output, session) {
  
  source("srv-demo.R", local = TRUE)
  
  
  # login status and info will be managed by shinyauthr module and stores here
  credentials <- callModule(shinyauthr::login, "login",
                            data = user_base,
                            user_col = user,
                            pwd_col = password,
                            sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))
  
  # logout status managed by shinyauthr module and stored here
  logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
  
  # this opens or closes the sidebar on login/logout
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  
  
 
  
  observeEvent(input$help2,
               introjs(session, options = list("nextLabel"="Siguiente",
                                               "prevLabel"="Regresar",
                                               "skipLabel"="Salir",
                                               "doneLabel"="Aceptar",steps=boton()
               ),
               events = list(onbeforechange = readCallback("switchTabs"))
               ))
  
  
  
  
  
  Datos_score <- reactive(data.frame(
  
  element=c("#paso1","#paso2","#paso3","#paso4","#paso5","#paso6","#paso7","#paso8","#paso9"),
  
  intro=c("Datos de ejemplo para crear el score de crédito",
          "Introduce tus propios datos para crear tu propio score de crédito",
          "Escoge la variable que indica el estado del cliente",
          "Datos de ejemplo para crear proyecciones sobre clientes a partir del score de crédito",
          "Datos propios para crear proyecciones sobre clientes a partir del score de crédito",
          "Datos de ejemplo para crear el rating de crédito",
          "Introduce tus propios datos para crear tu propio rating de crédito",
          "A partir de los datos del score proyectar ratings a los clientes del score",
          "Datos propios para crear proyecciones sobre clientes a partir del rating de de crédito"
          ),
  
  data.position=c("bottom","bottom","bottom","bottom","bottom","bottom","bottom","bottom","bottom")
))
  
  
  
  Datos_perdida <- reactive(data.frame(
    
    element=c("#paso10","#paso11","#paso12","#paso13"),
    
    intro=c("Datos de ejemplo para calcular la distribución de pérdidas de clientes, estos datos son históricos de pérdidas individuales de clientes.",
            "Introduce tus propios datos propios.",
            "Datos de ejemplo para calcular la distribución de pérdidas de clientes, estos datos son históricos de pérdidas de las clases crediticias de clientes.",
            "Introduce tus propios datos propios."),
    
    data.position=c("bottom","bottom","bottom","bottom")
  ))
  
  
  Datos_matriz <- reactive(data.frame(
    
    element=c("#paso14","#paso15"),
    
    intro=c("Datos de ejemplo para calcular la matriz de transición, estos datos son históricos de transiciones crediticias.",
            "Introduce tus propios datos propios."),
    
    data.position=c("bottom","bottom")
  ))
  
  Datos_cred1 <- reactive(data.frame(
    
    element=c("#paso16","#paso17","#paso18","#paso19","#paso20"),
    
    intro=c("Se deben cargar los datos correspondientes a la exposición crediticia de la cartera de clientes",
            "Se cargan las probabilidades de incumplimients de los clientes de la cartera, deben ser números entro 0 y 1",
            "Se cargan las probabilidades de incumplimients de los clientes de la cartera a partir del Score calculado.",
            "Se ingresa una pérdida esperada global a toda la cartera de clientes",
            "Si se posee, se cargan las pérdidas esperadas por cliente. Deben ser números entre 0 y 1 que represantan porcentajes."),
    
    data.position=c("bottom","bottom","bottom","bottom","bottom")
  ))
  
  Datos_cred2 <- reactive(data.frame(
    
    element=c("#paso21","#paso22","#paso23","#paso24","#paso25","#paso26"),
    
    intro=c("Datos de ejemplo para la exposición crediticia de la sección Credimetrics",
            "Datos propios, estos deben estat compuesto por dos columnas, una con la exposición y otra con la calificación crediticia",
            "Se carga la matriz calculada",
            "Se carga una matriz propia que represta las probabilidades de transición crediticia",
            "Se cargan las pérdidas por clases calculadas",
            "Se cargan las pérdidas por clases propias, deben ser números entre 0 y 1 que representen en porcentaje las pérdidas esperadas por clase"),
    
    data.position=c("bottom","bottom","bottom","bottom","bottom","bottom")
  ))
  
  
  
  Datos_bac <- reactive(data.frame(
    
    element=c("#paso27"),
    
    intro=c("Debe ingresar los datos correspondientes a la fecha, la pérdida esperada y la pérdida real"),
    
    data.position=c("bottom")
  ))
  
  Datos_raroc <- reactive(data.frame(
    
    element=c("#paso28","#paso29"),
    
    intro=c("Debe seleccionar la metodología de riesgo con la cual se calcularon las métricas de riesgo.",
            "Se ingresa el archivo con los datos contables del banco."),
    
    data.position=c("bottom","bottom")
  ))
  
  
  seccion_stats <- reactive(data.frame(
    
    element=c("#paso30","#paso31","#paso32","#paso33"),
    
    intro=c("En esta sección se selecciona la variable a comparar con la variable mora.",
            "En esta gráfica se muestra la relación con la variable una vez seleccionada.",
            "Se muestra la información estadística de la variable seleccionada.",
            "Se escogen los distintos tipos de  variables."),
    
    data.position=c("bottom","bottom","bottom","bottom")
  ))
  
  seccion_score <- reactive(data.frame(
    
    element=c("#paso34","#paso35","#paso36","#paso37","#paso38","#score","#download","#proy","#download1"),
    
    intro=c("Escoga el tipo de modelo para calcular el score",
            "Coeficientes de las variables independientes del modelo.",
            "Resumen estadístico del modelo",
            "Matriz de confusión del modelo, en la cual se aprecia la efectividad.",
            "El Gráfico ROC es una medida cualitativa del modelo",
            "Se muestra el score y la probabilidad de incumplimiento de los clientes que se usaron para entrenar el modelo.",
            "En este boton se permite descargar la data anterior.",
            "Se muestra la  score y la probabilidad de incumplimiento aplicados a nuevos clientes usando el modelo calculado.",
            "En este boton se permite descargar la data anterior."),
    
    data.position=c("bottom","bottom","bottom","bottom","bottom","bottom","bottom","bottom","bottom")
  ))
  
  seccion_rat <- reactive(data.frame(
    
    element=c("#paso39","#paso40","#download2"),
    
    intro=c("Una vez cargados los datos del modelo se muestra la información del mismo",
            "Proyección del rating de los clientes a través de su probabilidad de incumplimiento.",
            "En este boton se permite descargar la data anterior."),
    
    data.position=c("bottom","bottom","bottom")
  ))
  
  
  seccion_lgd <- reactive(data.frame(
    
    element=c("#paso41","#paso42","#paso43","#paso44","#paso45","#paso46","#paso47"),
    
    intro=c( 
            "Histograma de pérdidas históricas de los clientes",
            "Ingresar el número de submuestras para realizar la simulación",
            "Tamaños de las submuestras en comparación con la muestra total", 
            "Histograma de pérdidas al realizar las simulaciones", 
            "Promedio de pérdidas de las simulaciones", 
            "Nivel de fiabilidad para obtener el intervalo de confianza del parámetro de pérdida", 
            "Intervalo de confianza del parámetro pérdida" 
            ),
    
    data.position=c("bottom","bottom","bottom","bottom","bottom","bottom","bottom")
  ))
  
  
  seccion_lgd2 <- reactive(data.frame(
    
    element=c("#paso48","#paso49","#paso50","#paso51","#paso52"),
    
    intro=c( 
      "Pérdida promedio de cada clase",
      "Ingresar el número de submuestras para realizar la simulación",
      "Tamaños de las submuestras en comparación con la muestra total", 
      "Nivel de fiabilidad para obtener el intervalo de confianza del parámetro de pérdida", 
      "Resultados de la simulación" 
    ),
    
    data.position=c("bottom","bottom","bottom","bottom","bottom")
  ))
  
  seccion_mtr <- reactive(data.frame(
    
    element=c("#paso53"),
    
    intro=c( 
      "Una vez cargado los datos, se mostrara la matriz de transición crediticia, en ella se muestran las probabilidades de
      transición entre las clases"
      ),
    
    data.position=c("bottom")
  ))
  
  
  
  seccion_credimas <- reactive(data.frame(
    
    element=c("#paso54","#paso55","#paso56","#paso57","#paso58","#paso59","#paso60"
              ,"#paso61","#paso62","#paso63"),
    
    intro=c( 
      "Unidad de pérdida la cual se usa para agrupar los créditos por bandas de exposición",
      "Información crediticia necesaria para el cálculo de las métricas de riesgo",
      "Información de las probabilidades de incumplimiento de la cartera de clientes de la institución",
      "Distribución acumulada de probabilidad de número de incumplimientos esperados",
      "Información acerca de las bandas de exposición crediticia, con las cuales se agruparon los créditos",
      "Distribución acumulada de probabilidad de pérdida  esperadas",
      "Nivel de confianza para el cálculo de las métricas de riesgo",
      "Pérdida esperada de la cartera de clientes",
      "Valor en riesgo de la cartera de clientas",
      "TVaR o VaR estresado de la cartera de clientes"
    ),
    
    data.position=c("bottom","bottom","bottom","bottom","bottom","bottom","bottom","bottom","bottom","bottom")
    ))
  
  
  
  seccion_tvar <- reactive(data.frame(
    
    element=c("#paso64","#paso65","#paso66"),
    
    intro=c( 
      "Nivel de estres al VaR para realizar la prueba",
      "Resultado de la prueba estresada",
      "Podemos descargar un reporte con la información de la metodología de riesgo de crédito CreditRisk+"
    ),
    
    data.position=c("bottom","bottom","bottom")
    ))
  
  
  seccion_credimet <- reactive(data.frame(
    
    element=c("#paso67","#paso68","#paso69","#paso70","#paso71","#paso72","#paso73"),
    
    intro=c( 
      "Información crediticia necesaria para el cálculo de las métricas de riesgo",
      "Nivel de confianza para el cálculo de las métricas de riesgo",
      "Número de simulaciones para iniciar la prueba",
      "Histograma de pérdidas al realizar la simulación",
      "Pérdida esperada de la cartera de clientes",
      "Valor en riesgo de la cartera de clientes",
      "TVaR o VaR estresado de la cartera de clientes"
    ),
    
    data.position=c("bottom","bottom","bottom","bottom","bottom","bottom","bottom")
  ))
  
  
  
  
  seccion_tvar2 <- reactive(data.frame(
    
    element=c("#paso74","#paso75","#paso76"),
    
    intro=c( 
      "Nivel de estres al VaR para realizar la prueba",
      "Resultado de la prueba estresada",
      "Podemos descargar un reporte con la información de la metodología de riesgo de crédito Creditmetrics"
    ),
    
    data.position=c("bottom","bottom","bottom")
  ))
  
  seccion_back1 <- reactive(data.frame(
    
    element=c("#paso77","#paso78","#paso79","#report_back"),
    
    intro=c( 
      "Valor del parámetro para realizar la prueba",
      "Resultado del backtesting con las distintas metodologías",
      "Representación gráfica del backtesting",
      "Podemos descargar un reporte con los resultados del backtesting"
      
    ),
    
    data.position=c("bottom","bottom","bottom","bottom")
  ))
  
  acerca1 <- reactive(data.frame(
    
    element=c("#paso80"),
    
    intro=c( 
      "Información de contacto SynergyVision"
    ),
    
    data.position=c("bottom")
  ))
  
  Mor1 <- reactive(data.frame(
    
    element=c("#paso81","#paso82"),
    
    intro=c( 
      "Se presentan las formulas para la contrucción de cada indicador",
      "Se presentan las resultados de cada indicador"
    ),
    
    data.position=c("bottom","bottom")
  ))
  

  #CONDICIONALES CON TABITEMS
  boton <- reactive({
    if(input$tabs=="subitem3"){
      return(Datos_matriz())
    }else if(input$tabs=="resultados_back"){
      return(seccion_back1())
    }else if(input$tabs=="subitem1"){
      return(Datos_score())
    }else if(input$tabs=="subitem2"){
      return(Datos_perdida())
    }else if(input$tabs=="datini"){
      return(Datos_cred1())
    }else if(input$tabs=="CRED"){
      return(Datos_cred2())
    }else if(input$tabs=="datos_back"){
      return(Datos_bac())
    }else if(input$tabs=="RAROC"){
      return(Datos_raroc())
    }else if(input$tabs=="stat"){
      return(seccion_stats())
    }else if(input$tabs=="glm"){
      return(seccion_score())
    }else if(input$tabs=="rat"){
      return(seccion_rat())
    }else if(input$tabs=="lgd"){
      return(seccion_lgd())
    }else if(input$tabs=="CPC"){
      return(seccion_lgd2())
    }else if(input$tabs=="CMT"){
      return(seccion_mtr())
    }else if(input$tabs=="Param"){
      return(seccion_credimas())
    }else if(input$tabs=="ST1"){
      return(seccion_tvar())
    }else if(input$tabs=="RES"){
      return(seccion_credimet())
    }else if(input$tabs=="ST2"){
      return(seccion_tvar2())
    }else if(input$tabs=="acerca"){
      return(acerca1())
    }else if(input$tabs=="Mor"){
      return(Mor1())
    }else{}
    
    
  })
    

  
   observe( {
     toggle("panel1",condition = credentials()$user_aut)
     
  })
   
   observe( {
     toggle("panel2",condition = credentials()$user_aut)
     
   })
   
   observe( {
     toggle("panel3",condition = credentials()$user_aut)
     
   })
   
   observe( {
     toggle("panel4",condition = credentials()$user_aut)
     
   })
   
   observe( {
     toggle("panel5",condition = credentials()$user_aut)
     
   })
   
   observe( {
     toggle("panel6",condition = credentials()$user_aut)
     
   })
   
   observe( {
     toggle("panel7",condition = credentials()$user_aut)
     
   })
  
   observe( {
     toggle("panel9",condition = credentials()$user_aut)
     
   })
   
   observe( {
     toggle("panel10",condition = credentials()$user_aut)
     
   })
   observe( {
     toggle("panel11",condition = credentials()$user_aut)
     
   })
   observe( {
     toggle("panel12",condition = credentials()$user_aut)
     
   })
   observe( {
     toggle("panel13",condition = credentials()$user_aut)
     
   })
   observe( {
     toggle("panel14",condition = credentials()$user_aut)
     
   })
   observe( {
     toggle("panel15",condition = credentials()$user_aut)
     
   })
   observe( {
     toggle("panel16",condition = credentials()$user_aut)
     
   })
   observe( {
     toggle("panel17",condition = credentials()$user_aut)
     
   })
   observe( {
     toggle("panel18",condition = credentials()$user_aut)
     
   })
   observe( {
     toggle("panel19",condition = credentials()$user_aut)
     
   })
   observe( {
     toggle("panel20",condition = credentials()$user_aut)
     
   })
   
   
  
  observe({
    if(credentials()$user_auth) {
      V8::JS(js$hidehead(''))
    } else {
      V8::JS(js$hidehead('none'))
    }
  })
  
  
  # only when credentials()$user_auth is TRUE, render your desired sidebar menu
  output$sidebar <- renderMenu({
    
    if(credentials()$user_auth) {
      
    
    
    
    sidebarMenu(id = "tabs",
                
                menuItem("Datos", tabName = "datos", icon = icon("fal fa-database"),
                         menuSubItem("Scoring y Rating", tabName = "subitem1", icon = icon("circle-o")),
                         menuSubItem("Pérdida por Incumplimiento", tabName = "subitem2", icon = icon("circle-o")),
                         menuSubItem("Matriz de Transición", tabName = "subitem3", icon = icon("circle-o")),
                         menuSubItem("CreditRisk+", tabName = "datini", icon = icon("circle-o")),
                         menuSubItem("CreditMetrics", tabName = "CRED", icon = icon("circle-o")),
                         menuSubItem("Backtesting", tabName = "datos_back", icon = icon("circle-o")),
                         menuSubItem("Indicadores Contables", tabName = "RAROC", icon = icon("circle-o"))
                ),
                
                menuItem("Scoring y Rating", tabName = "S_R", icon = icon("fal fa-database"),
                         menuSubItem("Estadísticos", tabName = "stat", icon = icon("circle-o")),
                         menuSubItem("Score de Crédito", tabName = "glm", icon = icon("circle-o")),
                         menuSubItem("Rating de Crédito", tabName = "rat", icon = icon("circle-o"))
                         
                ),
                
                
                menuItem("Pérdida por Incumplimiento", tabName = "LGD", icon = icon("fal fa-database"),
                         menuSubItem("Pérdida por Cliente", tabName = "lgd", icon = icon("circle-o")),
                         menuSubItem("Pérdida Por Clase", tabName = "CPC", icon = icon("circle-o"))
                         
                         
                ),
                
                
                menuItem("Matriz de Transición", tabName = "MT", icon = icon("fal fa-database"),
                         
                         menuSubItem("Matriz de Transición", tabName = "CMT", icon = icon("circle-o"))
                         
                ),
                
                
                menuItem("CreditRisk+", tabName = "data", icon = icon("fal fa-database"),
                         
                         
                         menuSubItem("Resultados", tabName = "Param", icon = icon("circle-o")),
                         menuSubItem("Stress Testing", tabName = "ST1", icon = icon("circle-o"))
                ),
                
                
                menuItem("Creditmetrics", icon = icon("fal fa-database"), tabName = "crm",
                         
                         menuSubItem("Simulación y Resultados", tabName = "RES", icon = icon("circle-o")),
                         menuSubItem("Stress Testing", tabName = "ST2", icon = icon("circle-o"))
                         
                ),
                menuItem("Backtesting", icon = icon("fal fa-database"), 
                         
                         menuSubItem("Resultados", tabName = "resultados_back", icon = icon("circle-o"))
                ),
                
                menuItem("Indicadores Contables", icon = icon("exclamation-circle"), tabName = "raroc",
                         menuSubItem("Indicadores Contables", tabName = "Mor", icon = icon("circle-o"))
                         
                ),
                
                
                menuItem("Acerca", icon = icon("exclamation-circle"), tabName = "acerca"),
                
                
                  actionButton("help2", "Instrucciones")
                
                )
  
    } else { menuItem("nada", tabName = "nada", icon = icon("fal fa-database")
                     
    ) }
  })
  
  user_info <- reactive({credentials()$info})
  
  output$bienvenida <- renderText({
    req(credentials()$user_auth)
    
    glue("Bienvenid@ {user_info()$name}")
  })
  
  
  ##### Scoring y reting
  
  
  
##### Scoring  
  
  
  
  
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
    if(input$dataset && !input$userFile){
      data <- datasetSelect()}
    
    else if(!input$dataset && input$userFile){
      data <- datasetInput()
    }
  })
  
  
  ####Se muestran los datos
  
  
  output$datatable<-renderDataTable({
    data1org()
  },options = list(scrollX=T,scrollY=300))
  
  
  
  
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
    
    
    if (class(ca7)[1]=="try-error") {
      
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
      
      c()
      
    }else{ca8}
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  ### funcion que calcula los pvalores de las variables cuanlitativas
  
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
      
      c()
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
      
      c()
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
  
  
  
  
  ###############Funcion que crea el modelo para el score
  
  modprueba <- function(datos,datos2,nom,linkm,selectdir,split)  {
    
    s1 <- datos
    
    nombres <- colnames(datos2)
    
    nombre <- nom
    
    posi <- which(nombres == nombre)
    
    
    ceros <- subset(s1, s1[,posi]==0)
    unos <- subset(s1, s1[,posi]==1)
    
    
    indices0 <- sample( 1:nrow( ceros ), nrow(ceros)*(1-split) )
    ceros.muestreado <- ceros[ indices0, ]
    ceros.test <- ceros[-indices0,]
    
    indices1 <- sample( 1:nrow( unos ), nrow(unos)*(1-split) )
    unos.muestreado <- unos[ indices1, ]
    unos.test <- unos[-indices1,]
    
    train <- rbind(ceros.muestreado,unos.muestreado)
    test <- rbind(ceros.test,unos.test)
    
    colnames(train)[posi] <- "dependiente"
    colnames(test)[posi] <- "dependiente"
    
    
    
    if (selectdir=="backward") {
      modelo <- glm(dependiente ~. , data = train, family = binomial(link = linkm))
      reduccion = step(modelo)
    }else if(selectdir=="forward") {
      
      nothing <- glm(dependiente ~ 1 , data = train, family = binomial(link = linkm))
      fullmod <- glm(dependiente ~. , data = train, family = binomial(link = linkm))
      
      
      reduccion = step(nothing,
                      scope=list(lower=formula(nothing),upper=formula(fullmod)), direction="forward")
      
    }else if(selectdir=="both") {
      
      nothing <- glm(dependiente ~ 1 , data = train, family = binomial(link = linkm))
      fullmod <- glm(dependiente ~. , data = train, family = binomial(link = linkm))
      
      
      reduccion = step(nothing,
                       scope=list(lower=formula(nothing),upper=formula(fullmod)), direction="both")
      
    }
   
    
    return(list(reduccion,train,test))
    
  }
  
  
  ### modelo
  GlmModel <- reactive({
    
    
    input$goButton3
    
    isolate(modprueba(data1(),data1org(),input$columns,input$radio1,input$stepp, as.numeric(input$div1)))
    
  }
  )
  
  
  
  
  
  ###########funcion que muestra los parametros de la regresión
  coefglm <- function(modelo){
    
    l1 <- modelo$coefficients
    
    l2 <-names(modelo$coefficients)
    
    res <- t(rbind(l2,l1))
    
    res[1,1] <- "Punto de corte con el eje Y"
    colnames(res) <- c("Variables","Coeficientes")
    
    return(res)
    
  }
  
  #### Se muestran los coeficientes del modelo
  
  
  
  output$coefglm <- renderDataTable({
    
    ca139 <- try( coefglm(GlmModel()[[1]]))
    if (class(ca139)=="try-error") {
      
      c()
    }else{ca139}

    
  })
  
  
  #####Se muestra información estadistica del modelo
  
  ### funcion que la ordena
  estglm <- function(modelo){
    
    aic <- round(modelo$aic,2)
    
    ND <- round(modelo$null.deviance,2)
    
    RD <- round(modelo$deviance)
    
    fis <- modelo$iter
    
    Est <- c(aic,RD,ND,fis)
    
    inf <- data.frame(c("Criterio de información de Akaike","Desviación de los residuos","Desviación Nula","Número de iteraciones de Fischer"),Est)
    colnames(inf) <- c("Estadísticos","Resultado")
    
    return(inf)
    
  }
  
  
  ### aqui se muesrtra
  
  output$estglm <- renderDataTable({
    
    ca139 <- try(estglm(GlmModel()[[1]]))
    if (class(ca139)=="try-error") {
      
      c()
    }else{ca139}
    
    
  })
  
  
  
  
  #####Aque se calcula la matriz de confusion del modelo
  
  calaccur <- reactive(
    {
      
      
     
      pdata <- predict(GlmModel()[[1]], newdata = GlmModel()[[3]], type = "response")
      
      pred <- confusionMatrix(data = as.factor(as.numeric(pdata>0.5)), reference = as.factor(GlmModel()[[3]]$dependiente))
      
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
    
    
    model <-  GlmModel()[[1]]
    
    log_predict <- predict(model,newdata = GlmModel()[[3]],type = "response")
    
    log_predict <- ifelse(log_predict > 0.5,1,0)

    
    pr <- prediction(log_predict,GlmModel()[[3]]$dependiente)
    perf <- performance(pr,measure = "tpr",x.measure = "fpr") 
    plot(perf,ylab="proporción de verdaderos positivos",
         xlab="proporción de falsos positivos",xlim=c(0,1),ylim=c(0,1))
    abline(0,1,col="red")

  })
  
  
  ###########Funcion que calcula el score y pd de toda la cartera
  
  scor <- function() {
    
    
    s1 <- data1()
    nombres <- colnames(data1org())
    
    nombre <- input$columns
    
    posi <- which(nombres == nombre)
    
    
    reduccion = GlmModel()[[1]]
    
    
    Score <- predict(reduccion, newdata = s1, type = "link")
    PD <- predict(reduccion, newdata = s1, type = "response")
    n <- length(PD)
    ress <- cbind(1:n,Score,PD)
    colnames(ress) <- c("Posición","Score","Probabilidad de incumplimiento") 
    return(ress)
    
  }
  
  ######Se muestra el score de toda la cartera
  
  
  
  output$score <-renderDataTable({
    
    ca16 <- try(scor()[,c(2,3)])
    if (class(ca16)=="try-error") {
      
      c()
    }else{ca16}
    
    
  },options = list(scrollX=T,scrollY=300))
  
  
  output$download <- downloadHandler(
    filename = function(){"score.csv"}, 
    content = function(fname){
      write.csv(scor(), fname)
    }
  )
  
  
  output$split <- renderDataTable({
    
    ca16 <- try(GlmModel()[[2]])
    if (class(ca16)=="try-error") {
      
      c()
    }else{ca16}
    
    
  },options = list(scrollX=T,scrollY=300))
  
  
  output$download10 <- downloadHandler(
    filename = function(){"split.csv"}, 
    content = function(fname){
      write.csv(GlmModel()[[2]], fname)
    }
  )
  
  output$split1 <- renderDataTable({
    
    ca16 <- try(GlmModel()[[3]])
    if (class(ca16)=="try-error") {
      
      c()
    }else{ca16}
    
    
  },options = list(scrollX=T,scrollY=300))
  
  
  output$download11 <- downloadHandler(
    filename = function(){"split.csv"}, 
    content = function(fname){
      write.csv(GlmModel()[[3]], fname)
    }
  )
  
  
  ### Preyeccion a nuevos clientes
  
  
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
    if(input$datasetr && !input$userFiler){
      data <- datasetSelectr()
      
    }
    
    else if(!input$datasetr && input$userFiler) {
      
      data <- datasetInputproy()
    }
  })
  
  
  
  
  
  ### Se muestran los datos
  
  
  output$datatabler<-renderDataTable({
    dataaa2()
  })
  
  ## la proyeccion
  
  
  ## Aqui se muestra el score proyectado
  output$proy <- renderDataTable({
    
    
    
    
    reduccion = GlmModel()[[1]]
    
    
    Score <- predict(reduccion, newdata = dataaa2(), type = "link")
    PD <- predict(reduccion, newdata = dataaa2(), type = "response")
    n <- length(PD)
    ress <- cbind(1:n,Score,PD)
    colnames(ress) <- c("Posición","Score","Probabilidad de incumplimiento") 
    ress
   
    
  },options = list(scrollX=T,scrollY=300))
  
  
  
  output$download1 <- downloadHandler(
    filename = function(){"score.csv"}, 
    content = function(fname){
      write.csv(proyec(), fname)
    }
  )
  
  
  ######## rating
  
  
  
  
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
  
  
  

  datasetSelectRat <- reactive({
    datasetSelect <- rat
  })
  
  
  ###### Cargando datos con que se trabajara: entre los de ejemplo y los propios
  
  dataRat <- reactive({
    if(input$datasetRat && !input$userFileRat){
      data <- datasetSelectRat()}
    
    else if(!input$datasetRat && input$userFileRat){
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
  
  
  ## se calcula el modelo
  mod_rat <- reactive(lda_mo(dataRat()))
  
  
  ###########Funcion que calcula la informacion del modelo
  
  infLda <- function(lda){
    
    prior <- round(lda$prior,4)
    mea <- t(round(lda$means,4))
    inf <- rbind(prior,mea)
    inf <- cbind(c("Probabilidades a Priori","Valores Medio de los Grupos"),inf)
    return(inf)
  }
  
  ### Se muestra la informacion del modelo
  
  output$datatableRatInf <- renderDataTable({
    
    ca1342 <- try(infLda(mod_rat()))
    
    if (class(ca1342)=="try-error") {
      
      "Cargue datos"
      
    }else{ca1342}
  })
  
  ## Se proyectan a nuevos clientes
  
  
  
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
  
  
  ###### Cargando datos con que se trabajara: entre los del score y los propios
  
  dataRatN <- reactive({
    if(input$datasetRatN && !input$userFileRatN){
      data <- datasetSelectRatN()}
    
    else if(!input$datasetRatN && input$userFileRatN){
      data <- datasetInputRatN()
    }
  })
  
  output$datos_proyect <- renderDataTable({
    dataRatN()
    
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
  
  ## la proyeccion
  
  proyrat <- reactive({
    
    l1 <- as.data.frame(dataRatN()[,3])
    colnames(l1) <- c("Probabilidad de incumplimiento")
    
    
    l2 <- Rat_Cli(mod_rat(),as.data.frame(dataRatN()[,3]))
    
    
    l3 <- cbind(l1,l2)
    l3
    
  })
  
  
  
  #### Se muestra la proyeccion
  
  
  output$datatableRatNCF <- renderDataTable({
    
    proyrat()
    
    
    
  },options = list(scrollX=T,scrollY=300))
  
  ## Se descarga la proyeccion
  
  output$download2 <- downloadHandler(
    filename = function(){"score.csv"}, 
    content = function(fname){
      write.csv(proyrat(), fname)
    }
  )
  
  
  
  
  
  
  
  
  
  ##### Perdida por incumplimiento
  
  #### Perdida por cliente
  
 
  
  ######################## Datos perdida por clientes
  
  
  
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
    if(input$datasetrl && !input$userFilerl){
      data <- datasetSelectrl()}
    
    else if(!input$datasetrl && input$userFilerl){
      data <- datasetInputrl()
    }else{data.frame()}
  })
  
  
  ### esta parte se encarga de mostrar los datos
  
  output$datatablerl<-renderDataTable({
    data7()
  },options = list(scrollX=T,scrollY=300))
  
  
  ######## lgd1 se encarga de hacer la grafica
  
  lgd1 <- reactive({
    
    lgd <-data7()["Perdidas"]
    
    p7 <- ggplot(lgd, aes(x = Perdidas)) +
      geom_histogram()+labs(x = "Pérdida",y = "Frecuencia")
    
    
    
    return( ggplotly(p7))
    
  })
  
  ### aqui se muestra la grafica.
  
  output$curvalgd <- renderPlotly({
    
    ca13 <- try(lgd1())
    
    
    if (class(ca13)[1]=="try-error") {
      
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
  
  
  
  
  bot <- reactive({
    
    input$goButton5
    
    isolate(bootL(data7(),input$boot))
  })
  
  ### aqui se muestra la grafica.
  
  output$booot1 <- renderPlotly({
    
    
    
    ca9879 <- try(bot()[[1]])
    
    
    if (class(ca9879)[1]=="try-error") {
      
      df <- data.frame()
      ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
      
    }else{ca9879}
    
    
  })
  
  ## Informcaion de bootstrap
  
  
  output$boots3 <- renderUI({
    my_calculated_value <- round(as.numeric(bot()[[2]])*100,2)
    withMathJax(paste0("El valor promerdio de perdida cuando un cliente esta Default es: $$", my_calculated_value,"$$"))
  })
  
  output$boots4 <- renderUI({
    media <- round(as.numeric(bot()[[2]])*100,2)
    des <- round(as.numeric(bot()[[3]])*100,2)
    
    withMathJax(paste0("El intevalo de confianza al " ,input$boot23,"% para este valor : $$(", round(media+(qnorm((1-(as.numeric( input$boot23)/100 ))/2))*des/sqrt(as.numeric(input$boot)),2),",", round(media-(qnorm((1-(as.numeric( input$boot23)/100 ))/2)*des/sqrt(as.numeric(input$boot))),2),")$$"))
  }) 
  
  #### datos perdidas por clases
  
  ######################## Datos perdida por clases
  
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
    if(input$datasetC && !input$userFileC){
      data <- datasetSelectC()}
    
    else if(!input$datasetC && input$userFileC){
      data <- datasetInputC()
    }else{
      data.frame()
      
    }
  })
  
  ### Se muestran los datos
  
  output$datatableC<-renderDataTable({
    data11()
  },options = list(scrollX=T,scrollY=300))
  
  
  ## Funcion que calcula la perdida promedio por clase
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
  
  ### Se muestra la perdida por clases promedio
  
  datasetInputcrm1 <- reactive({
    datasetInputcrm1 <-CR(data11())
  })
  

  output$datatablecrm1<-renderDataTable({
    
    ca22 <- try(datasetInputcrm1())
    
    
    if (class(ca22)=="try-error") {
      
      c()
      
    }else{ca22}
    
    
    
    
    
  })
  
  
  
### Perdida por clases usando botstrap
  
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
  
  clasescal <- eventReactive(input$goButton500,{
    
    bostCL(data11(), as.integer(input$bootC) , as.integer(input$bootTC)) 
    
    
  })
  
  clasecrm <- reactive({
    
    l <- as.data.frame(clasescal()[[1]])
    k <- as.data.frame(clasescal()[[2]])
    
    j <- k*qnorm((1-(as.numeric(input$boot2312)/100 ))/2)/sqrt(as.numeric(input$bootC))
    
    
    iz <- l+j
    de <- l-j
    final <- cbind(l,iz,de)
    
    colnames(final) <- c("Pérdida","Mínimo","Máximo")
    
    d <- cbind(clasescal()[[3]],round(final,2))
    colnames(d)[1] <- "Clase" 
    d
  })
  
  
  
  
  output$datatablecrm2<-renderDataTable({
    
    ca22 <- try(clasecrm())
    
    
    if (class(ca22)=="try-error") {
      
      c()
      
    }else{ca22}
    
    
    
    
    
  })
  
  
  
  
  
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 ####### Seccion datos
  
      #############Scoring y rating
  
            ########Scoring
  
                    ###### Se cargan los datos entre los de ejemplo y los propios

  
  
  
  ############ Proyeccion score
  
 
  
  #########Rating
  

  

  
  ################### Proyeccion rating
  
  ############# Aqui se calcula el rating de nuevos clientes
  
  

  
  
 
  

  
  ######### seccion matriz de transicion
  
  
  
  
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
    if(input$datasetMT && !input$userFileMT){
      data <- datasetSelectMT()}
    
    else if(!input$datasetMT && input$userFileMT){
      data <- datasetInputMT()
    }else{data.frame()}
  })
  
  output$datatableMT<-renderDataTable({
    data10()
  },options = list(scrollX=T,scrollY=300))
  
  
  ########## Datos creditrisk+
  
  ####################exposiciones
  
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
    if(input$datasetPro && !input$userFilePro){
      data <- datasetSelectPro()
      data <- as.data.frame(data)
      data <- data[3]
    }
    
    else if(!input$datasetPro && input$userFilePro){
      data <- datasetInputPro()
    }else{data.frame()}
  })
  
  ###Se muestran los datos
  
  output$datatablePro<-renderDataTable({
    
    ca22 <- try( Proba())
    
    
    if (class(ca22)=="try-error") {
      
      c()
      
    }else{ca22}
    
    
    
    
    
  },options = list(scrollX=T,scrollY=300))
  
  
  #######Probabilidades
  
  
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
  perdi_crrisk <- reactive({
    
    if(!input$PerdiGene && input$userFilePerd){
      
      datasetInputPer()
    }else{data.frame()}
    
    
  })
  
  output$PerdidaPropia<-renderDataTable({
    perdi_crrisk()
  },options = list(scrollX=T,scrollY=300))
  
  
  
  ####### CreditMetrics#######################
  
  
  #########Exposiciones
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
    if(input$dataset0 && !input$userFile0){
      data <- datasetSelect0()}
    
    else if(!input$dataset0 && input$userFile0){
      data <- datasetInput0()
    }else(data.frame())
  })
  
  
  output$datatable0<-renderDataTable({
    data6()
  },options = list(scrollX=T,scrollY=300))
  
  
  ####  matriz de transicion
  
  
  MatrizPropias <- reactive({
    
    inFiler <- input$file_datacrm
    
    if (is.null(inFiler))
      return(NULL)
    read.table(inFiler$datapath, header = input$headecrm,
               sep = input$sepcrm, quote = input$quotecrm)
    
  })
  
  
  #### data4 contiene ka matriz de seleccion calculada
  
  data4 <- reactive({
    if(input$datasetcrm && !input$userFilecrm){
      
      
      mattrans()
      
      
    }
    
    else if(!input$datasetcrm && input$userFilecrm){
      data <- MatrizPropias()
    }else{data.frame()}
  })
  
  ###SE muestra a matriz de transicion
  
  
  output$datatablecrm<-renderDataTable({
    
    ca22<- try( data4())
    
    
    if (class(ca22)=="try-error") {
      
      c()
      
    }else{ca22}
    
    
    
    
    
  },options = list(scrollX=T,scrollY=300))
  
  ####### Perdidas por clases
  
  
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
  
  
  
  
  datasetSelect_Cla_Cr <- reactive({
    datasetSelect <- clasecrm()
  })
  
  
  ###### Cargando datos con que se trabajara: entre los de ejemplo y los propios
  
  data_Cla_Cr <- reactive({
    if(input$dataset_Cla_Cr && !input$userFile_Cla_Cr){
      data <- datasetSelect_Cla_Cr()}
    
    else if(!input$dataset_Cla_Cr && input$userFile_Cla_Cr){
      data <- datasetInput_Cla_Cr()
    }else{data.frame()}
  })
  
  
  ####Se muestran los datos
  
  
  
  
  
  output$datatable_Cla_Cr<-renderDataTable({
    
    ca22 <- try( data_Cla_Cr())
    
    
    if (class(ca22)=="try-error") {
      
      c()
      
    }else{ca22}
    
    
    
    
    
  },options = list(scrollX=T,scrollY=300))
  
  
  
  
  ####### backstesting
  
  
  
  data_back <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file_data_back
    
    if (is.null(inFile))
      return(NULL)
    
    # read.table(inFile$datapath, header = input$header,
    #            sep = input$sep, quote = input$quote)
    a <- read.delim2(inFile$datapath, header = input$header_back,
                     sep = input$sep_back, quote = input$quote_back)
    
    return(a)
    
  })
  
  
  output$datatable_back<-renderDataTable({
    if(is.null(data_back())){return()}
    datatable(data_back())
  })
  
  
  
  
  ########## datos indicadores contables
  
  
  datasetInputindices <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
   
    read.table("data/Datos_Ejemplos/indicadoresContables/contable.csv", header = TRUE,
               sep = ";", quote = "")
    
  })
  
  output$datatableind<-renderDataTable({
    datasetInputindices()
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
        
  
    
        ####  subseccion Seleccion de variables
  
      
      ### Se calculan los p-valores de las variables cualitativas con la funcion pval
 
   
   
  
 ########### Seccion perdida por incumplimiento.
 
 
 ####Se cargan los datos de la manera usual
 
 
 

 
 

 
 
 

###### Seccion score de credito
 
 
#############sub seccion seleccion y resultdos del modelo
 
 
 
 
 
 
 
 
 
 
 
 


 

 ############### Subseccion Score de la cartera de credito de entrenamento
 
 
 ######## subcesion Proyeccion a nuevos clientes
 
 #### Se cargan los datos de la manera usual
 
 

 
 
 
 
 

 
 
 
 
   
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
   
   
   
   
   reduccion = GlmModel()[[1]]
   
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
 

 
 

 
 
 
 
 ### Baandas
 
 
 
 
 bandas <- function(uni,EAD,LGD){
   
   res <- ceiling((EAD*LGD)/uni)
   
   res1 <- as.data.frame(table(res))
   
   colnames(res1) <- c("Unidades de Pérdidas","Frequencia")
   
   return(res1)
   
   
 }  
 
 ######### perididas
 
 
 
 
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
   
   resul <- cbind(l2,l3)
   
   colnames(resul) <-c("Exposición (EAD)", "Probabilidad de incumplimiento (PI)",
                      "Pérdida dado el incumplimiento (LGD)" ,
                      "Pérdida esperada (EL)", "Unidades de pérdida")
   resul

 }) 
 
 
 
 

 
 
 output$perclien <- renderDataTable({
   
   ca18 <- try( perclienv())
   
   
   if (class(ca18)=="try-error") {
     
     c()
     
   }else{ ca18}
   
   
 },options = list(scrollX=T,scrollY=300))
 
 
 
 probinc <- reactive({
   
   l <-  as.data.frame((-1*lfactorial(1:1000)))+ (as.data.frame(1:1000)*log(sum(Proba())))-(sum(Proba())*log(exp(1)))
   
   l1 <- exp(l) 
   
   colnames(l1) <- "Probabilidad de  Incumplimiento"
   l2 <- as.data.frame(1:1000)
   
   colnames(l2)  <- "Numero de Incumplimientos"
   return(cbind(l2,l1))
   
   
   
 })
 
 
 
 
 
 
 
 output$numincum <- renderDataTable({
   
   ca18 <- try( probinc())
   
   
   if (class(ca18)=="try-error") {
     
     c()
     
   }else{ ca18}
   
   
   
 
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
   
   
   return(list(ggplot(final, aes(y=Probabilidad,x=Incumplimientos)) + geom_point(),sum(probinc()[1:1000,2]*num)))
   
   
   
  
   
 })
 
 
 output$comparacion1 <- renderPlotly({
   
   
   
   ca7 <- try(ggplotly(disn()[[1]]))
   
   
   if (class(ca7)[1]=="try-error") {
     
     df <- data.frame()
     ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
     
   }else{ca7}
   
   
   
   
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
 
 output$Perd23<-renderDataTable({
   
   ca22 <- try(perd23v())
   
   
   if (class(ca22)=="try-error") {
     
     c()
     
   }else{ca22}
   
   
   
   
   
 },options = list(scrollX=T,scrollY=300))
 
 
 #### Distribución de perdida de la cartera
 
 
 
 bandas1 <- reactive({
   
   
   
   l1 <- 1:100000
   l2 <- rep(0,100000) 
   bandas <- cbind(l1,l2)
   
   
   for(i in 2:(dim(perd23v())[1])) {
     
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
   
   
   return(list(acum,ggplot(final, aes(y=Probabilidad,x=Pérdida)) + geom_point()))
   
   
   
   
   
 })
 
 

 
 output$comparacion2 <- renderPlotly({
   
   
   
   ca7 <- try(ggplotly(disn2()[[2]]))
   
   
   if (class(ca7)[1]=="try-error") {
     
     df <- data.frame()
     ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
     
   }else{ca7}
   
   
   
 })
 
 
 
### METRICAS DE RIESGO CREDITRISK +
 
 
 
 metr <- eventReactive(input$goButtonvar,{
   
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
 
 Stress <-eventReactive(input$goButtonstre,{
   
   
   
   
   pro <- percarSt()
   
   n <- 0:(length(pro)-1)
   
   pe <- sum(pro*n)*input$uniper
   
 
  
   #varS <-  min(which(disn2St() > (as.numeric(input$conf)/100)))*input$uniper
   
   
   return(pe)
   
   
    
 })
 
 output$Stress <- renderText({
   
   ca18 <- try( Stress())
   
   
   if (class(ca18)=="try-error") {
     
     "Cargue datos"
     
   }else{paste(format(ca18*input$uniper,big.mark=".",scientific = FALSE),"Bs")}
  
   
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
   }else{paste(format(ca*input$uniper,big.mark=".",scientific = FALSE),"Bs")}
   
   
   
 })
 
 
 #################Se muestra el var
 
 output$var <- renderText({
   
   
   ca1 <- try(metr()[[2]])
   if (class(ca1)=="try-error") {
     
     "Cargue datos y seleccione parametros"
   }else{paste(format(ca1*input$uniper,big.mark=".",scientific = FALSE),"Bs")}
   
 
 })

 
 ########## Se muestra el Tvar
 
 output$tvar <- renderText({
   
   
   ca2 <- try(metr()[[3]])
   if (class(ca2)=="try-error") {
     
     "Cargue datos y seleccione parametros"
   }else{paste(format(ca2*input$uniper,big.mark=".",scientific = FALSE),"Bs")}
   
   
   
   
 })
 
 
 ########## Aqui se realiza el reporte
 
 output$reporte1 <- downloadHandler(
   
   filename = "reporte1.pdf",
   content = function(file){
     tempReport <- file.path(tempdir(),"reporte1.Rmd")
     file.copy("reporte1.Rmd", tempReport, overwrite = TRUE)
     params <- list(var1= input$uniper, var2=disn()[[1]],var3=disn()[[2]],
                    var4=disn2()[[2]],var5=metr(),var6=input$conf1, var7=input$estres2,
                    var8=Stress())
     
     
     
     
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
 
 

 
 
 
 
 
## Seccion matriz de transicion
 
 
 ##### Subseccion calculo de la matriz de transicion+
 
 ##### Se cargan y se muestran los datos relacionados para el calculo de la matriz de transicion
 
 
 
 
 
 
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
 
 mattrans <- reactive({ MTR(data10())})
 
 
 output$datatableMTR<-renderDataTable({
   
   ca22 <- try( t(mattrans()))
   
   
   if (class(ca22)=="try-error") {
     
     c()
     
   }else{ca22}
   
   
   
   
   
 },options = list(scrollX=T,scrollY=300))
 
 
 
 
 
 
 
 
 
 ### Seccion perdida por clase
 
 ##### Subseccion calculo de las pérdidas esperadas
 
 ### Se cargan los datos
 
 
 #####
 
 
 

 
 
 
 
 
 #### Subseccion perdida esperada
 
  


 
 
 
 

  
  ############# Seccion simulacion y resultados
 
 
 #### calvar calcula las metricas de riesgo
 
 
 calvar <-eventReactive(input$goButtonSim,{
    
   
     
   
     ## Matriz de transicion
     MT <- data4()
     ## Perdida por clase
     
     RP <- data_Cla_Cr()
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
          
          per[k] <- l[k,"creditos"]*as.numeric(RP[which(RP[,"Clase"]==trans[k]),"Pérdida"])/100
          
        }
        
        acum[i] <- sum(per)
        
        
      }
      
      Total[j] <- sum(acum)
      
      }
      
      peresp <- mean(Total)
      
      var <- mean(Total)+(sd(Total)*qnorm(as.numeric(input$conf1)/100))
      
      tvar <- mean(Total)+((sd(Total)*dnorm(qnorm(as.numeric(input$conf1)/100)))/(1-(as.numeric(input$conf1)/100)))
      
      strescr <- mean(Total)*(as.numeric(input$stress3)+1)
       
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
   
   
   if (class(ca7)[1]=="try-error") {
     
     df <- data.frame()
     ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
     
   }else{ca7}
   
   
   
 })
 
 
  ##### Se muestra la perdida esperada
 
 output$pe122 <- renderText({
   ca4 <- try(calvar()[[1]])
   if (class(ca4)=="try-error") {

     "Cargue datos y seleccione parametros"
   }else{paste(format(ca4, big.mark=".",scientific = FALSE),"bs",sep = " ")} })

   
   
   
 
 
 #### Se muestra el var
 
 
 
 output$var122 <- renderText({

   ca5 <- try(calvar()[[2]])
   if (class(ca5)=="try-error") {

     "Cargue datos y seleccione parametros"
   }else{paste(format(ca5,big.mark=".",scientific = FALSE),"Bs",sep = " ")}

 })

 
 
 
 #### Se muestra el Tvar

  
 output$tvar122 <- renderText({

   ca6 <- try(calvar()[[3]])
   if (class(ca6)=="try-error") {

     "Cargue datos y seleccione parametros"
   }else{paste(format(ca6,big.mark=".",scientific = FALSE),"Bs",sep = " ")}
   
 })
  
 
 
 
 
######## El reporte de credimetrics
 
 output$reporte2 <- downloadHandler(
   
   filename = "reporte2.pdf",
   content = function(file){
     tempReport <- file.path(tempdir(),"reporte2.Rmd")
     file.copy("reporte2.Rmd", tempReport, overwrite = TRUE)
     params <- list(var1 = data4(),
                    var2 = data_Cla_Cr(),
                    var3 = data6(),
                    var4 = input$simcrm,
                    var5 = calvar(), 
                    var6=input$conf1,
                    var7 = calvar(),
                    var8=input$stress3
                    )
     
     
     
     
     rmarkdown::render(tempReport,output_file = file,params = params, envir = new.env(parent = globalenv()))
   }
 )
 
  
  
### Seccion Stresting
 
 ##### Simplemente se llama a clavar
 output$Stres45 <- renderText({


   ca23 <- try(calvar()[[4]])


   if (class(ca23)=="try-error") {

     "Cargue datos"

   }else{paste(format(ca23,big.mark=".",scientific = FALSE),"Bs",sep = " ")}

 })
 #  
  
  
################### Indicadores contables#################
 
 
 ### Return On Assets
 
 Roa <- reactive({
   
   
   ## R es el resultado contable de los activos financieros
   ## A es el valor contable de los activos financieros
   R <- as.numeric(datasetInputindices()[4,2])
   A <- as.numeric(datasetInputindices()[5,2])
   
   return(R/A)
   
 })
 
 
 
 ### RAR  Resultado ajustado al riesgo
 
 RAR <- reactive({
   
   ## R es el resultado contable de los activos financieros
   R <- as.numeric(datasetInputindices()[4,2])
   
   if (input$meto==2) {
     
     per <- as.numeric(calvar()[[1]])

   }else if(input$meto==1){
     
     
     per <- metr()[[1]]
     
   }
   
   return(R-per)
   
   
   
 })
 
 
 ##Raroc risk adjusted return on capital
 
 raroc <- reactive({
   
   
   if (input$meto==2) {
     
     varr <- as.numeric(calvar()[[2]])
     
     
     
   }else if(input$meto==1){
     
     varr <-  metr()[[2]]
  
   }
   
   return(RAR()/varr)
   
   
 })
 
 
 ## RORAC
 
 rorac <- reactive({
   
   
   if (input$meto==2) {
     
     ## tvar
     tarr <- as.numeric(calvar()[[3]])
     
     
     
   }else if(input$meto==1){
     
     ## tvar 
     tarr <-  metr()[[3]]
     
   }
   
   
    # r es resultado contable de los activos financieros
   
   
   r <- as.numeric(datasetInputindices()[4,2])
   return(r/tarr)
   
   
 })
 
 ## rarorac –Risk Adjusted Return On Risk Adjusted Capital 
 
 rarorac <-reactive({
   
   
   
   if (input$meto==2) {
     
     ## tvar
     tarr <- as.numeric(calvar()[[3]])
     
     
     
   }else if(input$meto==1){
     
     ## tvar 
     tarr <-  metr()[[3]]
     
   }
   
   
   return(RAR()/tarr)
   
 })
 
  
  
 
 
 
  output$raroc1 <- renderText ({
    
    ca23 <- try(raroc())
    
    
    if (class(ca23)=="try-error") {
      
      "Cargue datos"
      
    }else{ca23}
    
    
    
  })
  
  
  output$roracc <- renderText ({
    
    ca23 <- try(rorac())
    
    
    if (class(ca23)=="try-error") {
      
      "Cargue datos"
      
    }else{ca23}
    
    
    
  })
  
  
  
  
  output$rar <- renderText ({
    
    
    ca23 <- try(RAR())
    
    
    if (class(ca23)=="try-error") {
      
      "Cargue datos"
      
    }else{ca23}
    
    
    
    
  })
  
  
  
  output$raroracc <- renderText ({
    
    ca23 <- try(rarorac())
    
    
    if (class(ca23)=="try-error") {
      
      "Cargue datos"
      
    }else{ca23}
    
    
    
    
  })
  

  
  
  
  
  
  
 
  
  
  
  
  
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
  
  
  
  
  ###############################################################################
  ###############################################################################
  
 
  #PORCENTAJE DEL VAR
  output$back_porcentaje <- renderPrint({as.numeric(sub(",",".",input$porback))})
  
  
  #funcion auxiliar Backtesting
  source(paste(getwd(),"script","kup1.R",sep = "/"))
  
  #muestro resultado Backtesting
  output$result_back <- renderPrint({
    data <- data_back()
    
    if(length(data[,1])!=0){
      
      #convierto a fecha
      data[,1] <- as.Date(as.character(data[,1]),format="%d/%m/%Y")
      #convierto en numero
      data[,2] <- as.numeric(as.character(data[,2]))
      data[,3] <- as.numeric(as.character(data[,3]))
      
      #diseño data frame para usarlo en kup1
      #numero de observaciones
      data$obs <- seq(1,nrow(data))
      
      #VaR -
      data$var_menos <- rep(0,nrow(data))
      for(i in 1:(nrow(data)-1)){
        data$var_menos[i] <- data[i+1,3]-data[i+1,2]
      }
      
      
      #VaR +
      data$var_mas <- rep(0,nrow(data))
      for(i in 1:(nrow(data)-1)){
        data$var_mas[i] <- data[i+1,3]+data[i+1,2]
      }
      
      #ordeno data
      data <- data[,c(4,1,2,5,3,6)]
      #names(data) <- c("obs","fecha","var","var_menos","posicion","var_mas")
      names(data) <- c("V1","V2","V3","V4","V5","V6")
      
      
      #uso funcion kup1
      #a <- kup1(data,0.05)
      a <- kup1(data,(1-as.numeric(sub(",",".",input$porback))))
      #return(a)
    }else{}
  })
  
  #GRAFICO BACKTESTING
  output$grafico_back <- renderPlotly({
    if(is.null(data_back())){return()}
    data <- data_back()
    
    if(length(data[,1])!=0){
      
      #convierto a fecha
      data[,1] <- as.Date(as.character(data[,1]),format="%d/%m/%Y")
      #convierto en numero
      data[,2] <- as.numeric(as.character(data[,2]))
      data[,3] <- as.numeric(as.character(data[,3]))
      
      #diseño data frame para usarlo en kup1
      #numero de observaciones
      data$obs <- seq(1,nrow(data))
      
      #VaR -
      data$var_menos <- rep(0,nrow(data))
      for(i in 1:(nrow(data)-1)){
        data$var_menos[i] <- data[i+1,3]-data[i+1,2]
      }
      
      
      #VaR +
      data$var_mas <- rep(0,nrow(data))
      for(i in 1:(nrow(data)-1)){
        data$var_mas[i] <- data[i+1,3]+data[i+1,2]
      }
      
      #ordeno data
      data <- data[,c(4,1,2,5,3,6)]
      #names(data) <- c("obs","fecha","var","var_menos","posicion","var_mas")
      names(data) <- c("V1","V2","V3","V4","V5","V6")
      
      
      #uso funcion kup1
      #a <- kup1(data,0.05)
      a <- kup1(data,(1-as.numeric(sub(",",".",input$porback))))
      #return(a)
    }else{}
    
    par <- a[[2]]
    #df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
    #                                     dose=rep(c("D0.5", "D1", "D2"),2),
    #                                     len=c(6.8, 15, 33, 4.2, 10, 29.5))
    df2 <- data.frame(Valores=rep(c("Estadísticos", "Valores críticos"), each=3),
                      Test=rep(c("Test Kupiec", "Test de Haas", "Test mixto"),2),
                      Valor=c(as.numeric(par[1,]),as.numeric(par[2,])))
    
    p <- ggplot(data=df2, aes(x=Test, y=Valor, fill=Valores)) +
      geom_bar(stat="identity")+scale_fill_brewer(palette="Paired")
    
    p
  })
  
  #funcion auxiliar para reporte
  back <- reactive({
    data <- data_back()
    
    if(length(data[,1])!=0){
      
      #convierto a fecha
      data[,1] <- as.Date(as.character(data[,1]),format="%d/%m/%Y")
      #convierto en numero
      data[,2] <- as.numeric(as.character(data[,2]))
      data[,3] <- as.numeric(as.character(data[,3]))
      
      #diseño data frame para usarlo en kup1
      #numero de observaciones
      data$obs <- seq(1,nrow(data))
      
      #VaR -
      data$var_menos <- rep(0,nrow(data))
      for(i in 1:(nrow(data)-1)){
        data$var_menos[i] <- data[i+1,3]-data[i+1,2]
      }
      
      
      #VaR +
      data$var_mas <- rep(0,nrow(data))
      for(i in 1:(nrow(data)-1)){
        data$var_mas[i] <- data[i+1,3]+data[i+1,2]
      }
      
      #ordeno data
      data <- data[,c(4,1,2,5,3,6)]
      #names(data) <- c("obs","fecha","var","var_menos","posicion","var_mas")
      names(data) <- c("V1","V2","V3","V4","V5","V6")
      
      
      #uso funcion kup1
      #kup1(data,0.05)
      kup1(data,(1-as.numeric(sub(",",".",input$porback))))
      
    }else{}
    
  })
  ###############################################################################
  ###############################################################################
  #################################    BACKTESTING    ###########################
  ###############################################################################
  ###############################################################################
  
  
  data_back <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    
    
    # read.table(inFile$datapath, header = input$header,
    #            sep = input$sep, quote = input$quote)
    a <- read.delim2("data/Datos_Ejemplos/BackTesting/backp.csv", header = FALSE,
                     sep = ";", quote = "")
    
    return(a)
    
  })
  
  
  output$datatable_back<-renderDataTable({
    if(is.null(data_back())){return()}
    #datatable(data()) %>% formatCurrency(1:3, 'Bs. ', mark = '.', dec.mark = ',')
    datatable(data_back())
  })
  
  # REPORTE BACKTESTING 
  
  output$report_back <- downloadHandler(
    filename = "reporte_back.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "reporte_back.Rmd")
      #tempReport <- file.path(getwd(), "reporte1.Rmd")
      
      file.copy("reporte_back.Rmd", tempReport, overwrite = TRUE)
      
      # Configuración de parámetros pasados a documento Rmd
      params <- list(resultados = back()
                     
                     
                     # data2 = data()$corriente,
                     # data3 = data()$corriente.rem,
                     # dist1 = input$distVarA,
                     # dist2 = input$distVarC,
                     # dist3 = input$distVarCR,
                     # pconf = input$porVar,
                     # reali = input$reali,
                     # revi = input$revi
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    })####
  
  
  ##########Seccion de requerimiento del venezuela
  
  
  
  reque <- reactive({
    
    
    
    l <- data6()
    perdidas_clases <-clasecrm()
    lgd <- l[2]
    
   
    
    lgdcli <- NULL
    
    for (i in 1:dim(lgd)[1]) {
      
      
      val <- perdidas_clases[perdidas_clases[1]==as.vector(lgd[i,1])]
      lgdcli[i] <- (as.numeric(val[2])/100)
      
    }
     
    
    res <- cbind(l,as.data.frame(lgdcli),l[1]*as.data.frame(lgdcli))
    colnames(res) <- c("Exposición (EAD)", "Calificación", "Perdida dado el incumplimiento (LGD)", "Pérdida esperada")
    res
    })
  
  
  
  
  output$datatable_per_credime <- renderDataTable({
    
    reque()
    
  })
  
  
  output$ex1 <- renderUI({
    withMathJax('$$\\textrm{RAROC}=\\frac{RAR}{VaR}$$')
  })
  
  output$ex2 <- renderUI({
    withMathJax('$$\\textrm{RAR}=\\textrm{resultados contables-Pérdida esperada}$$')
  })
  output$ex3 <- renderUI({
    withMathJax('$$\\textrm{RORAC}=\\frac{\\textrm{resultados contables}}{TVaR}$$')
  })
  output$ex4 <- renderUI({
    withMathJax('$$\\textrm{RARORAC}=\\frac{\\textrm{RAR}}{TVaR}$$')
  })
  
  output$ex5 <- renderUI({
    withMathJax('$$\\textrm{Ind. de Morosidad}=\\frac{\\textrm{Saldo Vencido}}{\\textrm{Saldo Total}}$$')
  })
  output$ex6 <- renderUI({
    withMathJax('$$\\textrm{Ind. de Cobertura}=\\frac{\\textrm{Saldo Vencido}}{\\textrm{Saldo preventivo para riesgo}}$$')
  })
  
  
  
  
  
  
  
  
  
})
