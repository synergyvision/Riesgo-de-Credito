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
                           menuSubItem("Estadísticos", tabName = "stat", icon = icon("circle-o")),
                           menuSubItem("Pérdida por incumplimiento", tabName = "lgd", icon = icon("circle-o")),
                           menuSubItem("Score de crédito", tabName = "glm", icon = icon("circle-o")),
                           menuSubItem("Parámetros y resultados", tabName = "Param", icon = icon("circle-o")),
                           menuSubItem("Stress Testing", tabName = "ST1", icon = icon("circle-o"))
                  ),
                  
                  
                  menuItem("Creditmetrics", icon = icon("th"), tabName = "crm",
                           menuSubItem("Créditos", tabName = "CRED", icon = icon("circle-o")),
                           menuSubItem("Matriz de transición", tabName = "CMT", icon = icon("circle-o")),
                           menuSubItem("Pérdida por clase", tabName = "CPC", icon = icon("circle-o")),
                           menuSubItem("Simulación y Resultados", tabName = "RES", icon = icon("circle-o")),
                           menuSubItem("Stress Testing", tabName = "ST2", icon = icon("circle-o"))
                           
                  ),
                  
                  menuItem("Indicadores contables", icon = icon("exclamation-circle"), tabName = "raroc",
                  menuSubItem("Cálculo del RAROC", tabName = "RAROC", icon = icon("circle-o")),
                  menuSubItem("Indicadores contables", tabName = "Mor", icon = icon("circle-o"))
                  
                  ),
                  
                  
                  menuItem("Acerca", icon = icon("exclamation-circle"), tabName = "acerca"))
    ),
    dashboardBody(VisionHeader(),
                  
                  tabItems(
                    tabItem(tabName = "intro",
                            
                            fluidRow(
                              column(4,box(title = "Vision Credit Risk" ,width = 112,background="yellow","Vision Credit Risk es una herramienta web que permite realizar la medición de las principales métricas de riesgo en el sector crediticio,
                                            tales como pérdida esperada, VaR, matrices de transición, recuperaciones luego del default, entre otras, de una manera sencilla, permitiendo al cliente tomar decisiones veloces gracias a la rapidez de los resultados, los cuales se basan en técnicas
                                            estadísticas precisas y de alta calidad, que son usadas y reconocidas a nivel mundial en el sector financiero.
                                           " )),column(8,div(img(src="img/cover.png",width=400),style="text-aling: center;")))
                            
                            ),
                    tabItem(tabName = "subitem1",
                            
                            
                           fluidRow(
                             fluidRow(column(6,box(width = 11,background="yellow", checkboxInput("dataset", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),column(6,box(width = 12,background="yellow", checkboxInput('userFile', strong('Cargar Datos Propios'), FALSE)))),
                            fluidRow(
                              column(12,box( background="yellow",width=12,status = "warning",
                                   selectInput('columns', 'Selecciona variable de estudio', "Seleccione primero los datos")
                              ))
                              
                            )),
                            conditionalPanel(condition = "input.userFile == true",
                                             fluidRow(
                                               box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                   box( width=15,background = "yellow",
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
                                       
                                       
                                       
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Relación de las variables independientes')),
                                                 fluidRow(
                                                   box( background="yellow",width=12,status = "warning",
                                                        selectInput('columns1', 'Selecciona variable de estudio', "Seleccione primero los datos")
                                                   )
                                                   
                                                 ),
                                                 
                                                 
                                                 
                                                 fluidRow(column(width=11,plotlyOutput("comparacion")),
                                                          box( width=12,title = "Resumen estadístico de la variable seleccionada",status = "warning",
                                                                                           dataTableOutput('estad1')
                                                 ))
                                                 
                                                 
                                                 
                                                 
                                                 
                                                 
                                       ),
                                       
                                       
                                       
                                       
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong("Selección de variables")),
                                                 fluidRow(
                                                   
                                                   box(width=12,status = "warning",checkboxGroupInput("selec", 
                                                                                                      h3("Tipo de selección"), 
                                                                                                      choices = list("Selección de variables cualitativas" = 1)),checkboxGroupInput("selec1" ,
                                                                                                                                                                                     h3(""), 
                                                                                                                                                                                     choices = list(
                                                                                                                                                                                                    "Selección de variables cuantitativas" = 2))),
                                                   conditionalPanel(condition = "(input.selec == 1)", box(style = "overflow-x:scroll",title = "Selección de variables cualitativas",width=12,status = "warning", numericInput("significancia","Ingrese el nivel de significancia",value = 0.05,min = 0.001,max=1),dataTableOutput('datatablecu'))),
                                                          
                                                   conditionalPanel(condition = "(input.selec1 == 2)",  box(style = "overflow-x:scroll",title = "Selección de variables cuantitativas",width=12,status = "warning", numericInput("significancia1","Ingrese el nivel de significancia",value = 0.05,min = 0.001,max=1),dataTableOutput('datatablecu1'))))
                                                  )
                               )
                             )
                    ),
                    tabItem(tabName = "glm",
                            
                            fluidRow(
                              tabBox( height = "1250px", width = 12,side = "left",
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Selección y resultados del modelo')),
                                                
                                               fluidRow( 
                                                box(width=12, title =h2("Modelos de probabilidad lineal"),solidHeader = T,status = "warning",radioButtons("radio1", h3("Escoga el link del Modelo"),
                                                                                            choices = list("Modelo Probit" = "probit", "Modelo Logit" = "logit",
                                                                                                           "Modelo Cauchit" = "cauchit"),selected = "probit")),
                                                
                                                fluidRow(column(width=6,box(width=10, status="primary",tableOutput("accur"))),column(width = 6,plotOutput("roc")))
                                                
                                                
                                      )),
                                      
                                      
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Score de la cartera de crédito')),
                                                
                                                h2("Score y probabilidad de incumplimiento de los clientes"),
                                                dataTableOutput("score")
                                               
                                                
                                                
                                                
                                                
                                                
                                      ),
                                      
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Proyección a nuevos clientes')),
                                                
                                                h2("Score y probabilidad de incumplimiento del nuevo cliente"),
                                                
                                                fluidRow(column(6,box(background="yellow",width = 200, checkboxInput("datasetr", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),column(6,box(background="yellow", width = 200,checkboxInput('userFiler', strong('Cargar Datos Propios'), FALSE)))),
                                                
                                               conditionalPanel(condition = "input.userFiler == true",
                                                                   fluidRow(
                                                                     box(width = 15, title = h3("Cargar el archivo con los datos"),
                                                                         box( width=15,background = "yellow",
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
                                                                         
                                                                     ))
                                                                   )),
                                                fluidRow(
                                                  box(width=12,style = "overflow-x:scroll",status = "warning",dataTableOutput('datatabler'))
                                                )
                                                
                                                
                                                
                                      )
                                      
                                      
                                      
                              ))),
                    
                    
                    tabItem(tabName = "lgd",
                            
                            fluidRow(
                              tabBox(
                                height = "1250px", width = 12,side = "left", 
                                
                                
                                tabPanel( title = tagList(shiny::icon("gear"), strong('Pérdidas por incumplimiento')),h3("Pérdidas por incumplimiento"),
                            fluidRow(column(6,box(background="yellow",width = 112, checkboxInput("datasetrl", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))), column(6,box(background="yellow",width = 112, checkboxInput('userFilerl', strong('Cargar Datos Propios'), FALSE)))),
                           
                            conditionalPanel(condition = "input.userFilerl == true",
                                             fluidRow(
                                               box(width = 15, title = h3("Cargar el archivo con los datos"),
                                                   box( width=15,background = "yellow",
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
                            ), 
                            plotlyOutput("curvalgd")
                            
                            
                            
                            )))),
                    
                    tabItem( tabName = "Param",
                             fluidRow(
                               tabBox( height = "1250px", width = 12,side = "left",
                             
                     
                            
                            tabPanel( title = tagList(shiny::icon("gear"), strong('Parametros iniciales')),
                                      h3("Modelo CreditRisk+"),
                                      fluidRow(column(6, box(width = 120, background="yellow",status = "warning", numericInput("uniper","Ingrese unidad de pérdida",value = 1000))),column(6,box(width = 200, background="yellow",status = "warning", numericInput("uni","Ingrese porcentaje de recuperación luego del default",value = 80)))),
                                     
                                      
                                      
                                      
                                      fluidRow(
                                        column(6,box(background="yellow", width = 120,checkboxInput("datasetr1", strong("Probabilidades de incumplimiento provenientes del Score"), FALSE))),
                                        column(6,box(background="yellow", width = 200,checkboxInput('userFiler1', strong("Probabilidades de incumplimiento propias"), FALSE)))),
                                  
                                      
                                       conditionalPanel(condition = "input.userFiler1 == true",
                                                         fluidRow(
                                                           box(width = 15, title = h3("Cargar el archivo con las probabilidades de incumplimiento"),
                                                               box( width=15,background = "yellow",
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
                                                         )),
                            tabPanel( title = tagList(shiny::icon("gear"), strong('Resultados')),
                                     
                                       fluidRow(box(title =h3("Escoga nivel de confianza para el VaR"),solidHeader = T,width=12,status = "warning",radioButtons("conf", "",
                                                                                                                choices = list("90%" = 90, "95%" = 95,
                                                                                                                               "99%" = 99),selected = 95)),   
                                               
                                               
                                               fluidRow(column(4,box(width=12,status = "warning",h3("Pérdida esperada"), h2(textOutput("pe")) )),column( 4,box(width=12,status = "warning",h3("Valor en rieso"), h2(textOutput("var"))  )),column(4,box(width=12,status = "warning",h3("TVaR"), h2(textOutput("tvar"))))),
                                               fluidRow( column( 12,box(width=12,h2("Reporte"), status = "warning",downloadButton("reporte1","Descargar")))))
                                      
                                      
                                      
                                                          )))),
                    tabItem(tabName = "ST1",
                            
                            fluidRow(column(6,box(width=12,title = h2("StressTesting"),solidHeader = T,status = "warning",radioButtons("estres2", h3("Nivel de estrés de la prueba"),
                                                                                                                              choices = list("1 %" = 0.01, "5 %" = 0.05,
                                                                                                                                             "10 %" = 0.1),selected = 0.01))),column(6,box(width=12,title = h2("Resultado"),solidHeader = T,status = "warning",h3("El valor de la prueba"),h1(textOutput("Stress")))))
                            
                            
                    )
                    
                    
                    
                    
                   ,
                    tabItem( tabName = "CRED",
                             h1("Créditos"),
                             
                             fluidRow(column(6,box(background="yellow",width = 200, checkboxInput("dataset0", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),column(6,box(background="yellow", width = 200,checkboxInput('userFile0', strong('Cargar Datos Propios'), FALSE)))),
                             
                             conditionalPanel(condition = "input.userFile0 == true",
                                              fluidRow(
                                                box(width = 15, title = h3("Cargar el archivo con los créditos"),
                                                    fluidRow(box( width=12,background = "yellow",
                                                         fileInput('file_datacrm0', 'Seleccione el archivo', accept = c('text/csv',
                                                                                                                        'text/comma-separated-values',
                                                                                                                        'text/tab-separated-values',
                                                                                                                        'text/plain',
                                                                                                                        '.csv',
                                                                                                                        '.tsv',
                                                                                                                        '.rda'),
                                                                   placeholder = 'Aun no seleccionas el archivo...', buttonLabel = 'Buscar' )
                                                    ),
                                                    
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
                   
                   
                    tabItem( tabName = "CMT",fluidRow(
                      tabBox( height = "1250px", width = 12,side = "left",
                              
                              
                              
                              
                              
                              tabPanel( title = tagList(shiny::icon("gear"), 
                                                        
                      strong('Calculo de la Matriz de transición')),
                    
                      
                    box(width = 120, h1('Calculo de la Matriz de transición')),
                            fluidRow(column(6,box(background="yellow",width = 200, checkboxInput("datasetMT", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),column(6,box(background="yellow", width = 200,checkboxInput('userFileMT', strong('Cargar Datos Propios'), FALSE)))),
                            
                            conditionalPanel(condition = "input.userFileMT == true",
                                             
                                               box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                   fluidRow( box( width=12,background = "yellow",
                                                        fileInput('file_dataMT', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                                  placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                                   ),
                                                   
                                                     box(width=4,background="yellow",strong(ENCABEZADO_TEXT),
                                                         checkboxInput( width="80%", 'headerMT', WITHHEADER_TEXT, TRUE)),
                                                     box(width=4,background="yellow",
                                                         radioButtons( width="40%", 'sepMT', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                                     box(width=4,background="yellow",
                                                         radioButtons( width="40%", 'quoteMT', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                                   )
                                               )
                                             ),
                            fluidRow(
                              box(style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatableMT')),
                              box(title = h3("Matriz de transición"),style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatableMTR'))
                            )
                      
                      
                           
                              
                            
                    ),
                    
                    tabPanel( title = tagList(shiny::icon("gear"), strong('Selección de la Matriz de transición')),
                              
                              box(width = 120, h2("Matriz de probabilidades de transición")),
                              fluidRow( column(width=6,box(background="yellow",width = 200, checkboxInput("datasetcrm", strong("Matriz de transición calculada"), FALSE))),column(width=6,box(background="yellow",width = 200, checkboxInput('userFilecrm', strong("Matriz de transición propia"), FALSE)))),
                              conditionalPanel(condition = "input.userFilecrm == true",
                                              
                                                 box(width = 15, title = h3("Cargar el archivo con la matriz de transición"),
                                                     fluidRow( box( width=12,background = "yellow",
                                                          fileInput('file_datacrm', 'Seleccione el archivo', accept = c('text/csv',
                                                                                                                        'text/comma-separated-values',
                                                                                                                        'text/tab-separated-values',
                                                                                                                        'text/plain',
                                                                                                                        '.csv',
                                                                                                                        '.tsv',
                                                                                                                        '.rda'),
                                                                    placeholder = 'Aun no seleccionas el archivo...', buttonLabel = 'Buscar' )
                                                     ),
                                                    
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
                                               
                                               
                              ),fluidRow(
                                box(width=12,status = "warning",dataTableOutput('datatablecrm')))
                              
                              
                             
                    )
                    
                    ))),
                    
                    
                    
                    
                    
                    tabItem( tabName = "CPC", fluidRow(
                      tabBox( height = "1250px", width = 12,side = "left",
                              
                              
                              
                              
                              
                              tabPanel( title = tagList(shiny::icon("gear"), strong('Calculo de las pérdidas esperadas')),
                             
                             
                             h1('Cálculo de las Pérdidas esperada por clases'),
                             fluidRow(column(6,box(background="yellow", width = 120,checkboxInput("datasetC", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),column(6,box(background="yellow",width = 120, checkboxInput('userFileC', strong('Cargar Datos Propios'), FALSE)))),
                           
                             conditionalPanel(condition = "input.userFileC == true",
                                           
                                               box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                    fluidRow(  box( width=12,background = "yellow",
                                                         fileInput('file_dataC', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                                   placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                                    ),
                                                   
                                                      box(width=4,background="yellow",strong(ENCABEZADO_TEXT),
                                                          checkboxInput( width="80%", 'headerC', WITHHEADER_TEXT, TRUE)),
                                                      box(width=4,background="yellow",
                                                          radioButtons( width="40%", 'sepC', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                                      box(width=4,background="yellow",
                                                          radioButtons( width="40%", 'quoteC', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, '')))
                                                    )
                                                )
                                              ,
                             fluidRow(
                               box(style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatableC'))
                             ),
                             fluidRow(
                               box(title = h3("Pérdida esperada por clase"),style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatableCR'))
                             )
                    ), tabPanel( title = tagList(shiny::icon("gear"), strong('Pérdida esperada')), 
                                 box(width = 15, title = h1("Pérdida esperada por clase")),
                                 fluidRow(column(6,box(background="yellow",width = 120,checkboxInput("datasetcrm1", strong("Pérdida por clases calculada"), FALSE)))
                                 ,column(6,box(background="yellow", width = 120,checkboxInput('userFilecrm1', strong("Ingresar pérdida esperada por clase"), F)))),
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
                                                    
                                                      box(width=4,background="yellow",strong("Encabezado de los datos"),
                                                          checkboxInput( width="80%", 'headecrm1', "Con encabezado", TRUE)),
                                                      box(width=4,background="yellow",
                                                          radioButtons( width="40%", 'sepcrm1', "Separador", c('Coma'=',',
                                                                                                               'Punto y coma'=';',
                                                                                                               'Tab'='\t'), ';')),
                                                      box(width=4,background="yellow",
                                                          radioButtons( width="40%", 'quotecrm1', "Comillas", c('Ninguna'='',
                                                                                                                'Comilla doble'='"',
                                                                                                                'Comilla simple'="'"), '')
                                                    )
                                                    
                                                  )
                                                  
                                 ),fluidRow(
                                   box(width=12,status = "warning",dataTableOutput('datatablecrm1'))))))
                    
                    
                    
                    ),
                    
                    tabItem( tabName = "RES" , 
                             
                             fluidRow(
                               
                               
                             box(h1('Simulación y Resultados'),width=8, status = "warning",numericInput("simcrm","Número de simulaciones",value = 2) ),
                             box(title = h3("Escoga nivel de confianza para el VaR"),width=12,solidHeader = T,status = "warning",radioButtons("conf1","", 
                                                                                          choices = list("90%" = 90, "95%" = 95,
                                                                                                         "99%" = 99),selected = 95)),
                             fluidRow(column(4, box(title = h3("Pérdida esperada:"),width=12,status = "warning", h3(textOutput("pe122")) )),
                                      column(4,box(title = h3("Valor en riesgo:"),width=12,status = "warning", h3(textOutput("var122")) )),
                                      column(4, box(title = h3("TVaR:"),width=12,status = "warning",h3(textOutput("tvar122")) 
                                                  ))),downloadButton("reporte2","Descargar")
                            
                             
                            )
                             
                             
                             
                             
                    ),
                   
                   tabItem(tabName = "ST2",
                           
                           fluidRow(column(6,box(width=12,title = h2("StressTesting"),solidHeader = T,status = "warning",radioButtons("stress3", h3("Nivel de estrés de la prueba"),
                                                                                  choices = list("1 %" = 0.01, "5 %" = 0.05,
                                                                                                 "10 %" = 0.1),selected = 0.1))),column(6,box(width=12,title = h2("Resultado"),solidHeader = T,status = "warning",h3("El valor de la prueba"),h1(textOutput("Stres45")))))
                           
                           
                           ),
                    
                    tabItem( tabName = "RAROC" , 
                             
                             fluidRow(
                               tabBox( height = "1250px", width = 12,side = "left",
                                       
                                       
                                       
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Cálculo del RAROC')),
                             fluidRow(box(title =h1('Calculo del RAROC' ),solidHeader = T,status = "warning", width = 12,hr(),withMathJax(), "La ecuacion estandar para el calculo del RAROC es: $$\\textrm{RAROC}=\\frac{RAR}{C}$$ Donde: $$RAR=\\textrm{Risk Adjusted Return}$$ $$C=\\textrm{Capital necesario para cumbrir el riesgo}$$" )
                            )),
                    tabPanel(title = tagList(shiny::icon("gear"), strong('RAROC')),
                          fluidRow(  
                          box(width=12,title = h3("Metodología"),solidHeader = T,status ="warning" ,checkboxGroupInput("meto",h3(""), 
                                                                                               choices = list("CreditRisk+" = 1, 
                                                                                                              "Credimetrics" = 2))),
                            fluidRow(column(6,box(width=12,background="yellow",fileInput("file", h4("Ingrese Balance"),placeholder = 'Aun no seleccionas el archivo...', buttonLabel = 'Buscar'))),
                            column(6,box(width=12,background="yellow",h3("RAROC"),h2(textOutput("Raroc1"))))),
                          
                          fluidRow(
                            box(width = 15, title = h3("Cargar el archivo con los datos contables"),
                                box( width=15,background = "yellow",
                                     fileInput('indices', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                               placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                ),
                                fluidRow(
                                  box(width=4,background="yellow",strong(ENCABEZADO_TEXT),
                                      checkboxInput( width="80%", 'headerind', WITHHEADER_TEXT, TRUE)),
                                  box(width=4,background="yellow",
                                      radioButtons( width="40%", 'sepind', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                  box(width=4,background="yellow",
                                      radioButtons( width="40%", 'quoteind', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                ),
                                
                                fluidRow(
                                  box( style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatableind'))
                                )
                            )
                          )
                          
                          )
                            
                            )
                    )
                    )),
                    
                   tabItem( tabName = "Mor" ,
                            fluidRow(column(4,box(width=12,background="yellow",h3("RAROC"),h3(textOutput("raro")))),column(4,box(width=12,background="yellow",h3("RORAC"),textOutput("roracc"))),column(4, box(width=12,background="yellow",h3("RARORAC"),textOutput("raroracc")))),
                            fluidRow(column(4,box(width=12,background="yellow",h3("Indice de Morósidad"),textOutput("morosidad"))),column(4,box(width=12,background="yellow",h3("Indice de Cobertura"),textOutput("cobertura"))),column(4, box(width=12,background="yellow",h3("RAR"),textOutput("rar"))))
                              
                            
                            

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
