shinyUI(
  dashboardPage(
    
    dashboardHeader(title = NULL, titleWidth = 188,
                    
                    dropdownMenu(type = "messages",
                                 
                                 messageItem(
                                   from = "Alerta",
                                   message = "Niveles de Riesgo Atípicos",
                                   icon = icon("exclamation-triangle"),
                                   time = "2018-05-12"
                                 ),
                                 
                                 messageItem(
                                   from = "Señal",
                                   message = "Volatilidad Anormal",
                                   icon = icon("life-ring"),
                                   time = "2018-05-12"
                                 )
                    )
    ),
    
    dashboardSidebar(
      
      sidebarSearchForm(label = "Ingrese un Número", "searchText", "searchButton"),
      
      
      sidebarMenu(id = "tabs",
      
                  menuItem("Introducción", tabName = "intro", icon = icon("fal fa-database")),
                              
                  menuItem("CreditRisk+", tabName = "data", icon = icon("fal fa-database"),
                           
                           
                           
                           menuSubItem("Datos", tabName = "subitem1", icon = icon("circle-o")),
                           menuSubItem("Estadisticos", tabName = "stat", icon = icon("circle-o")),
                           menuSubItem("Perdida por incumplimiento", tabName = "lgd", icon = icon("circle-o")),
                           menuSubItem("Modelo de probabiidad lineal", tabName = "glm", icon = icon("circle-o")),
                           menuSubItem("Parametros iniciales", tabName = "Param", icon = icon("circle-o")),
                           menuSubItem("Resultados del modelo", tabName = "Res", icon = icon("circle-o"))
                  ),
                  
                  
                  menuItem("Creditmetrics", icon = icon("th"), tabName = "crm",
                           menuSubItem("Créditos", tabName = "CRED", icon = icon("circle-o")),
                           menuSubItem("Calculo Matriz de transicion", tabName = "CMT", icon = icon("circle-o")),
                           menuSubItem("Matriz de transicion", tabName = "MT", icon = icon("circle-o")),
                           menuSubItem("Cálculo de perdida por clase", tabName = "CPC", icon = icon("circle-o")),
                           menuSubItem("Perdida por clase", tabName = "PC", icon = icon("circle-o")),
                           menuSubItem("Simulación y Resultados", tabName = "RES", icon = icon("circle-o"))
                           
                           
                  ),
                  
                  menuItem("Raroc", icon = icon("exclamation-circle"), tabName = "raroc",
                  menuSubItem("Calculo del RAROC", tabName = "RAROC", icon = icon("circle-o")),
                  menuSubItem("Valor del RAROC", tabName = "VRAROC", icon = icon("circle-o"))
                  ),
                  
                  
                  menuItem("Acerca", icon = icon("exclamation-circle"), tabName = "acerca"))
    ),
    dashboardBody(VisionHeader(),
                  
                  tabItems(
                    tabItem(tabName = "intro",
                            
                            fluidRow(
                              column(4,box(title = "Vision Credit Risk" ,width = 112,background="yellow","Vision Credit Risk es una herramienta web que permite realizar la medición de las principales metricas de riesgo en el sector crediticio,
                                            tales como perdida esperada, VaR, matrices de transición, recuperaciones luego del default, entre otras, de una manera sencilla, permitiendo al cliente tomar decisiones veloces gracias a la rapidez de los resultados, los cuales se basan en técnicas
                                            estadísticas precisas y de alta calidad, que son usadas y reconocidas a nivel mundial en el sector financiero.
                                           " )),column(8,div(img(src="img/cover.png",width=400),style="text-aling: center;")))
                            
                            ),
                    tabItem(tabName = "subitem1",
                            
                            
                            fluidRow(column(6,box(width = 112,background="yellow", checkboxInput("dataset", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),column(6,box(width = 112,background="yellow", checkboxInput('userFile', strong('Cargar Datos Propios'), FALSE)))),
                            
                            conditionalPanel(condition = "input.userFile == true",
                                             fluidRow(
                                               box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                   box( width=12,background = "yellow",
                                                        fileInput('file_data', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                                  placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                                   ),
                                                   fluidRow(
                                                     box(width=4,background="yellow",strong(ENCABEZADO_TEXT),
                                                         checkboxInput( width="80%", 'header', WITHHEADER_TEXT, TRUE)),
                                                     box(width=4,background="yellow",
                                                         radioButtons( width="40%", 'sep', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                                     box(width=4,background="yellow",
                                                         radioButtons( width="40%", 'quote', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                                   )
                                               )
                                             )),
                            fluidRow(
                              box( style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatable'))
                            )
                    ),
                    tabItem( tabName = "stat",
                             fluidRow(
                               tabBox( height = "1250px", width = 12,side = "left",
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Variable de estudio'))
                                                 ,box( background="yellow",width=12,status = "warning",
                                                       textOutput('variables')
                                                 )   ,
                                                 box( background="yellow",width=12,status = "warning",
                                                      numericInput("num", 
                                                                   h3("Seleccione la posicion de la variable de estudio"), 
                                                                   value = 1)
                                                 )  
                                                 
                                                 
                                                 
                                                 
                                       ),
                                       
                                       
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Relacion de las variables independientes')),
                                                 fluidRow(box( background="yellow",width=12,status = "warning",
                                                               textOutput('variables1')
                                                 ),
                                                 
                                                 box( background="yellow",width=12,status = "warning",
                                                      numericInput("num1", 
                                                                   h3("Seleccione la variable a comparar"), 
                                                                   value = 2)
                                                 ))
                                                 
                                                 ,
                                                 fluidRow(plotlyOutput("comparacion"))
                                                 
                                                 
                                                 
                                       ),
                                       
                                       
                                       
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Estadísticos Básicos')),
                                                 
                                                 box( background="yellow",width=12,status = "warning",
                                                      textOutput('varia23')
                                                 ),
                                                 
                                                 box( width=12,status = "warning",
                                                      dataTableOutput('estad1')
                                                 )
                                                 
                                       ),
                                       tabPanel( title = tagList(shiny::icon("gear"), strong("Selección de variables cualitativas ")),    
                                                  numericInput("significancia","Ingrese el nivel de significancia",value = 0.05,min = 0.001,max=1),
                                                 box(style = "overflow-x:scroll",width=12,status = "warning", dataTableOutput('datatablecu'))),
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong("Selección de variables cuantitativas ")),    
                                                 numericInput("significancia1","Ingrese el nivel de significancia",value = 0.05,min = 0.001,max=1),
                                                 box(style = "overflow-x:scroll",width=12,status = "warning", dataTableOutput('datatablecu1')))
                               )
                             )
                    ),
                    tabItem(tabName = "glm",
                            
                            fluidRow(
                              tabBox( height = "1250px", width = 12,side = "left",
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Seleccion del modelo')),
                                                
                                                h2("Modelos de probabilidad lineal"),
                                                box(width=12, background="yellow",radioButtons("radio1", h3("Escoga el link del Modelo"),
                                                                                            choices = list("Modelo Probit" = "probit", "Modelo Logit" = "logit",
                                                                                                           "Modelo Cauchit" = "cauchit"),selected = "probit")),
                                                h3("Convertir perfiles negativos al valor 1 y perfiles positivos al valor 0")
                                                ,box(width=12, background="yellow",radioButtons("radio2", h3("Escoga una opcion"),
                                                                                             choices = list("Cambiar" = 1, "Mantener por defecto" = 2),selected = 2))
                                      ),
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Resultados del modelo')),
                                                
                                                
                                                
                                                
                                                box(width=5, background="yellow",tableOutput("accur")
                                                ),
                                                box(width=12, background="yellow",plotOutput("roc")
                                                )),
                                      
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Score de la cartera de credito')),
                                                
                                                h2("Score y probabilidad de incumplimiento de los clientes"),
                                                dataTableOutput("score")
                                               
                                                
                                                
                                                
                                                
                                                
                                      ),
                                      
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Proyeccion a nuevos clientes')),
                                                
                                                h2("Score y probabilidad de incumplimiento del nuevo cliente"),
                                                
                                                fluidRow(box(background="yellow", checkboxInput("datasetr", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),
                                                fluidRow(box(background="yellow", checkboxInput('userFiler', strong('Cargar Datos Propios'), FALSE)))
                                                , conditionalPanel(condition = "input.userFiler == true",
                                                                   fluidRow(
                                                                     box(width = 15, title = h3("Cargar el archivo con los datos"),
                                                                         box( width=12,background = "yellow",
                                                                              fileInput('file_datar', 'Seleccione el archivo', accept = c('text/csv',
                                                                                                                                          'text/comma-separated-values',
                                                                                                                                          'text/tab-separated-values',
                                                                                                                                          'text/plain',
                                                                                                                                          '.csv',
                                                                                                                                          '.tsv',
                                                                                                                                          '.rda'),
                                                                                        placeholder = 'Aun no seleccionas el archivo...', buttonLabel = 'Buscar' )
                                                                         ),
                                                                         fluidRow(
                                                                           box(width=4,background="yellow",strong("Encabezado de los datos"),
                                                                               checkboxInput( width="80%", 'headerr', "Con encabezado", TRUE)),
                                                                           box(width=4,background="yellow",
                                                                               radioButtons( width="40%", 'sepr', "Separador", c('Coma'=',',
                                                                                                                                 'Punto y coma'=';',
                                                                                                                                 'Tab'='\t'), ';')),
                                                                           box(width=4,background="yellow",
                                                                               radioButtons( width="40%", 'quoter', "Comillas", c('Ninguna'='',
                                                                                                                                  'Comilla doble'='"',
                                                                                                                                  'Comilla simple'="'"), ''))
                                                                         )
                                                                     )
                                                                   )),
                                                fluidRow(
                                                  box(width=12,status = "warning",dataTableOutput('datatabler'))
                                                ), box(width=12,status = "warning",tableOutput('dat'))
                                                
                                                
                                                
                                      )
                                      
                                      
                                      
                              ))),
                    
                    
                    tabItem(tabName = "lgd",
                            
                            fluidRow(
                              tabBox(
                                height = "1250px", width = 12,side = "left", 
                                
                                
                                tabPanel( title = tagList(shiny::icon("gear"), strong('Perdidas por incumplimiento')),h3("Perdidas por incumplimiento"),
                            fluidRow(box(background="yellow", checkboxInput("datasetrl", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),
                            fluidRow(box(background="yellow", checkboxInput('userFilerl', strong('Cargar Datos Propios'), FALSE))),
                            conditionalPanel(condition = "input.userFilerl == true",
                                             fluidRow(
                                               box(width = 15, title = h3("Cargar el archivo con los datos"),
                                                   box( width=12,background = "yellow",
                                                        fileInput('file_datarl', 'Seleccione el archivo', accept = c('text/csv',
                                                                                                                    'text/comma-separated-values',
                                                                                                                    'text/tab-separated-values',
                                                                                                                    'text/plain',
                                                                                                                    '.csv',
                                                                                                                    '.tsv',
                                                                                                                    '.rda'),
                                                                  placeholder = 'Aun no seleccionas el archivo...', buttonLabel = 'Buscar' )
                                                   ),
                                                   fluidRow(
                                                     box(width=4,background="yellow",strong("Encabezado de los datos"),
                                                         checkboxInput( width="80%", 'headerrl', "Con encabezado", TRUE)),
                                                     box(width=4,background="yellow",
                                                         radioButtons( width="40%", 'seprl', "Separador", c('Coma'=',',
                                                                                                           'Punto y coma'=';',
                                                                                                           'Tab'='\t'), ';')),
                                                     box(width=4,background="yellow",
                                                         radioButtons( width="40%", 'quoterl', "Comillas", c('Ninguna'='',
                                                                                                            'Comilla doble'='"',
                                                                                                            'Comilla simple'="'"), ''))
                                                   )
                                               )
                                             )),
                            
                            fluidRow(
                              box(style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatablerl'))
                            )
                            )))),
                    
                    tabItem(tabName = "Param",
                            
                            tabPanel( title = tagList(shiny::icon("gear"), strong('Parametros iniciales')),
                                      h3("Modelo CreditRisk+"),
                                      box(width = 4, background="yellow",status = "warning", numericInput("uniper","Ingrese unidad de perdida",value = 1000)),
                                      box(width = 6, background="yellow",status = "warning", numericInput("uni","Ingrese porcentaje de recuperacion luego del default",value = 80)),
                                      
                                      box(width = 12, background="yellow",status = "warning",h2("Score y probabilidad de incumplimiento del nuevo cliente")),
                                      fluidRow( box(background="yellow", checkboxInput("datasetr1", strong("Probabilidades de incumplimiento provenientes del Score"), FALSE))),
                                      fluidRow(box(background="yellow", checkboxInput('userFiler1', strong("Probabilidades de incumplimiento propias"), FALSE)))
                                      
                                      , conditionalPanel(condition = "input.userFiler1 == true",
                                                         fluidRow(
                                                           box(width = 15, title = h3("Cargar el archivo con las probabiidades de incumplimiento"),
                                                               box( width=12,background = "yellow",
                                                                    fileInput('file_datar1', 'Seleccione el archivo', accept = c('text/csv',
                                                                                                                                 'text/comma-separated-values',
                                                                                                                                 'text/tab-separated-values',
                                                                                                                                 'text/plain',
                                                                                                                                 '.csv',
                                                                                                                                 '.tsv',
                                                                                                                                 '.rda'),
                                                                              placeholder = 'Aun no seleccionas el archivo...', buttonLabel = 'Buscar' )
                                                               ),
                                                               fluidRow(
                                                                 box(width=4,background="yellow",strong("Encabezado de los datos"),
                                                                     checkboxInput( width="80%", 'headerr1', "Con encabezado", TRUE)),
                                                                 box(width=4,background="yellow",
                                                                     radioButtons( width="40%", 'sepr1', "Separador", c('Coma'=',',
                                                                                                                        'Punto y coma'=';',
                                                                                                                        'Tab'='\t'), ';')),
                                                                 box(width=4,background="yellow",
                                                                     radioButtons( width="40%", 'quoter1', "Comillas", c('Ninguna'='',
                                                                                                                         'Comilla doble'='"',
                                                                                                                         'Comilla simple'="'"), ''))
                                                               )
                                                           )
                                                         )),fluidRow(
                                                           box(width=12,status = "warning",dataTableOutput('datatabler1'))
                                                         ))),
                    
                    
                    
                    
                    tabItem( tabName = "Res",
                             box(width=12,status = "warning",background="yellow",radioButtons("conf", h3("Escoga nivel de confianza para el VaR"),
                                                                                          choices = list("90%" = 90, "95%" = 95,
                                                                                                         "99%" = 99),selected = 95)),     
                             
                             box(width=12,status = "warning",background="yellow",h2("Perdida esperada"), textOutput("pe")),
                             box(width=12,status = "warning",background="yellow",h2("Valor en rieso"), textOutput("var")),
                             box(width=12,status = "warning",background="yellow",h2("TVaR"), textOutput("tvar")),
                             h2("Reporte"),
                             downloadButton("reporte1","Descargar")
                    ),
                    tabItem( tabName = "CRED",
                             h1("Creditos"),
                             
                             fluidRow(box(background="yellow", checkboxInput("dataset0", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),
                             fluidRow(box(background="yellow", checkboxInput('userFile0', strong('Cargar Datos Propios'), FALSE))),
                             conditionalPanel(condition = "input.userFile0 == true",
                                              fluidRow(
                                                box(width = 15, title = h3("Cargar el archivo con los creditos"),
                                                    box( width=12,background = "yellow",
                                                         fileInput('file_datacrm0', 'Seleccione el archivo', accept = c('text/csv',
                                                                                                                        'text/comma-separated-values',
                                                                                                                        'text/tab-separated-values',
                                                                                                                        'text/plain',
                                                                                                                        '.csv',
                                                                                                                        '.tsv',
                                                                                                                        '.rda'),
                                                                   placeholder = 'Aun no seleccionas el archivo...', buttonLabel = 'Buscar' )
                                                    ),
                                                    fluidRow(
                                                      box(width=4,background="yellow",strong("Encabezado de los datos"),
                                                          checkboxInput( width="80%", 'headecrm0', "Con encabezado", TRUE)),
                                                      box(width=4,background="yellow",
                                                          radioButtons( width="40%", 'sepcrm0', "Separador", c('Coma'=',',
                                                                                                               'Punto y coma'=';',
                                                                                                               'Tab'='\t'), ';')),
                                                      box(width=4,background="yellow",
                                                          radioButtons( width="40%", 'quotecrm0', "Comillas", c('Ninguna'='',
                                                                                                                'Comilla doble'='"',
                                                                                                                'Comilla simple'="'"), ''))
                                                    )
                                                )
                                              )
                             ),
                             fluidRow(
                               box(style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatable0'))
                             )
                             
                    ),   
                    tabItem(tabName = "CMT", h1('Calculo de la Matriz de transición'),
                            fluidRow(box(background="yellow", checkboxInput("datasetMT", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),
                            fluidRow(box(background="yellow", checkboxInput('userFileMT', strong('Cargar Datos Propios'), FALSE))),
                            conditionalPanel(condition = "input.userFileMT == true",
                                             fluidRow(
                                               box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                   box( width=12,background = "yellow",
                                                        fileInput('file_dataMT', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                                  placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                                   ),
                                                   fluidRow(
                                                     box(width=4,background="yellow",strong(ENCABEZADO_TEXT),
                                                         checkboxInput( width="80%", 'headerMT', WITHHEADER_TEXT, TRUE)),
                                                     box(width=4,background="yellow",
                                                         radioButtons( width="40%", 'sepMT', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                                     box(width=4,background="yellow",
                                                         radioButtons( width="40%", 'quoteMT', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                                   )
                                               )
                                             )),
                            fluidRow(
                              box(style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatableMT'))
                            ),
                            fluidRow(
                              box(title = h3("Matriz de transicion"),style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatableMTR'))
                            )
                    ),
                    
                    tabItem( tabName="MT", h1('Matriz de transición')
                             ,box(width = 12, background="yellow",status = "warning",h2("Matriz de probabilidades de transicion de la cartera de clientes")),
                             fluidRow( column(width=5,box(background="yellow", checkboxInput("datasetcrm", strong("Matriz de transicion calculada"), FALSE))),column(width=5,box(background="yellow", checkboxInput('userFilecrm', strong("Matriz de transicion propia"), FALSE)))),
                             #fluidRow(box(background="red", checkboxInput('userFilecrm', strong("Matriz de transicion propia"), FALSE))),
                             conditionalPanel(condition = "input.userFilecrm == true",
                                              fluidRow(
                                                box(width = 15, title = h3("Cargar el archivo con la matriz de transicion"),
                                                    box( width=12,background = "yellow",
                                                         fileInput('file_datacrm', 'Seleccione el archivo', accept = c('text/csv',
                                                                                                                       'text/comma-separated-values',
                                                                                                                       'text/tab-separated-values',
                                                                                                                       'text/plain',
                                                                                                                       '.csv',
                                                                                                                       '.tsv',
                                                                                                                       '.rda'),
                                                                   placeholder = 'Aun no seleccionas el archivo...', buttonLabel = 'Buscar' )
                                                    ),
                                                    fluidRow(
                                                      box(width=4,background="yellow",strong("Encabezado de los datos"),
                                                          checkboxInput( width="80%", 'headecrm', "Con encabezado", TRUE)),
                                                      box(width=4,background="yellow",
                                                          radioButtons( width="40%", 'sepcrm', "Separador", c('Coma'=',',
                                                                                                              'Punto y coma'=';',
                                                                                                              'Tab'='\t'), ';')),
                                                      box(width=4,background="yellow",
                                                          radioButtons( width="40%", 'quotecrm', "Comillas", c('Ninguna'='',
                                                                                                               'Comilla doble'='"',
                                                                                                               'Comilla simple'="'"), ''))
                                                    )
                                                )
                                              )
                                              
                             ),fluidRow(
                               box(width=12,status = "warning",dataTableOutput('datatablecrm')))
                             
                             
                             
                             
                    ),
                    
                    
                    
                    tabItem( tabName = "CPC", h1('Calculo de las Perdidas esperada por clases'),
                             fluidRow(box(background="yellow", checkboxInput("datasetC", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),
                             fluidRow(box(background="yellow", checkboxInput('userFileC', strong('Cargar Datos Propios'), FALSE))),
                             conditionalPanel(condition = "input.userFileC == true",
                                              fluidRow(
                                                box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                    box( width=12,background = "yellow",
                                                         fileInput('file_dataC', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                                   placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                                    ),
                                                    fluidRow(
                                                      box(width=4,background="yellow",strong(ENCABEZADO_TEXT),
                                                          checkboxInput( width="80%", 'headerC', WITHHEADER_TEXT, TRUE)),
                                                      box(width=4,background="yellow",
                                                          radioButtons( width="40%", 'sepC', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                                      box(width=4,background="yellow",
                                                          radioButtons( width="40%", 'quoteC', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                                    )
                                                )
                                              )),
                             fluidRow(
                               box(style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatableC'))
                             ),
                             fluidRow(
                               box(title = h3("Perdida esperada por clase"),style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatableCR'))
                             )
                    ),   
                    
                    tabItem( tabName ="PC"  , h1('Perdida esperada por clase'),
                             box(width = 15, title = h1("Cargar el archivo con las perdida por clase")),
                             box(background="yellow",checkboxInput("datasetcrm1", strong("Perdida por clases calculada"), FALSE))
                             ,box(background="yellow", checkboxInput('userFilecrm1', strong("Ingresar perdida esperada por clase"), F)),
                             conditionalPanel(condition = "input.userFilecrm1 == true",
                                              fluidRow(
                                                
                                                box( width=12,background = "yellow",
                                                     fileInput('file_datacrm1', 'Seleccione el archivo', accept = c('text/csv',
                                                                                                                    'text/comma-separated-values',
                                                                                                                    'text/tab-separated-values',
                                                                                                                    'text/plain',
                                                                                                                    '.csv',
                                                                                                                    '.tsv',
                                                                                                                    '.rda'),
                                                               placeholder = 'Aun no seleccionas el archivo...', buttonLabel = 'Buscar' )
                                                ),
                                                fluidRow(
                                                  box(width=4,background="yellow",strong("Encabezado de los datos"),
                                                      checkboxInput( width="80%", 'headecrm1', "Con encabezado", TRUE)),
                                                  box(width=4,background="yellow",
                                                      radioButtons( width="40%", 'sepcrm1', "Separador", c('Coma'=',',
                                                                                                           'Punto y coma'=';',
                                                                                                           'Tab'='\t'), ';')),
                                                  box(width=4,background="yellow",
                                                      radioButtons( width="40%", 'quotecrm1', "Comillas", c('Ninguna'='',
                                                                                                            'Comilla doble'='"',
                                                                                                            'Comilla simple'="'"), ''))
                                                )
                                                
                                              )
                                              
                             ),fluidRow(
                               box(width=12,status = "warning",dataTableOutput('datatablecrm1')))),
                    
                    tabItem( tabName = "RES" , h1('Simulación y Resultados'),
                             box(width=4,background="yellow", numericInput("simcrm","suimulación",value = 2) ),
                             box(width=12,status = "warning",background="yellow",radioButtons("conf1", h3("Escoga nivel de confianza para el VaR"),
                                                                                          choices = list("90%" = 90, "95%" = 95,
                                                                                                         "99%" = 99),selected = 95)),
                             box(title = h1("La perdida esperada es:"),width=12,status = "warning",background="yellow", textOutput("pe122") ),
                             box(title = h1("El resultado del VaR es:"),width=12,status = "warning",background="yellow", textOutput("var122") ),
                             box(title = h1("El resultado del TVaR es:"),width=12,status = "warning",background="yellow", textOutput("tvar122") ,h2("Reporte"),
                                 downloadButton("reporte2","Descargar"))
                             
                             
                             
                             
                    ),
                    
                    tabItem( tabName = "RAROC" , h1('Calculo del RAROC' ),hr(),withMathJax(), "La ecuacion estandar para el calculo del RAROC es: $$\\textrm{RAROC}=\\frac{RAR}{C}$$ Donde: $$RAR=\\textrm{Risk Adjusted Return}$$ $$C=\\textrm{Capital necesario para cumbrir el riesgo}$$"),
                    tabItem(tabName = "VRAROC",
                            box(width=12,background="yellow",checkboxGroupInput("meto",h3("Metodología"), 
                                                                                               choices = list("CreditRisk+" = 1, 
                                                                                                              "Credimetrics" = 2))),
                            box(width=12,background="yellow",fileInput("file", h3("Ingrese Balance"),placeholder = 'Aun no seleccionas el archivo...', buttonLabel = 'Buscar')),
                            box(width=12,background="yellow",h3("El resultado del RAROC es"),textOutput("Raroc1"))
                            
                            ),
                    
                    
                    
                    tabItem(tabName = "acerca",
                            box( width = 9, status="warning",
                                 h3(ACERTITLE_TEXT),
                                 tags$hr(),
                                 h4(ACERVER_TEXT),
                                 h4(ACERRIF_TEXT),
                                 h4(ACERRS_TEXT),
                                 h4(ACERRS_TEXT2),
                                 tags$hr(),
                                 tags$img(src="img/visionrisk.png", width=300, align = "left"),
                                 br(),
                                 h5(ACERSUBSV_TEXT),
                                 br(),
                                 tagList(shiny::icon("map-marker"), ACERDIR_TEXT),br(),
                                 tagList(shiny::icon("phone"), ACERTLF_TEXT),br(),
                                 tagList(shiny::icon("envelope-o"), ACERCORR_TEXT)
                            )
                    )
                  )
    )
  )
)
