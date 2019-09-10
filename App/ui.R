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
      
                  menuItem("Datos", tabName = "datos", icon = icon("fal fa-database"),
                           menuSubItem("Scoring y rating", tabName = "subitem1", icon = icon("circle-o")),
                           menuSubItem("CreditRisk+", tabName = "datini", icon = icon("circle-o"))),
                  
                  menuItem("Scoring y rating", tabName = "S_R", icon = icon("fal fa-database"),
                           menuSubItem("Estadísticos", tabName = "stat", icon = icon("circle-o")),
                           menuSubItem("Score de crédito", tabName = "glm", icon = icon("circle-o")),
                           menuSubItem("Rating de crédito", tabName = "rat", icon = icon("circle-o"))
                           
                           ),
                  
                  
                  menuItem("Pérdida por incumplimiento", tabName = "LGD", icon = icon("fal fa-database"),
                           menuSubItem("Pérdida por clientes", tabName = "lgd", icon = icon("circle-o")),
                           menuSubItem("Pérdida Por Clase", tabName = "CPC", icon = icon("circle-o"))
                           
                           
                  ),
                              
                  menuItem("CreditRisk+", tabName = "data", icon = icon("fal fa-database"),
                           
                           
                           menuSubItem("Resultados", tabName = "Param", icon = icon("circle-o")),
                           menuSubItem("Stress Testing", tabName = "ST1", icon = icon("circle-o"))
                  ),
                  
                  
                  menuItem("Creditmetrics", icon = icon("th"), tabName = "crm",
                           menuSubItem("Créditos", tabName = "CRED", icon = icon("circle-o")),
                           menuSubItem("Matriz de transición", tabName = "CMT", icon = icon("circle-o")),
                           menuSubItem("Simulación y Resultados", tabName = "RES", icon = icon("circle-o")),
                           menuSubItem("Stress Testing", tabName = "ST2", icon = icon("circle-o"))
                           
                  ),
                  
                  menuItem("Indicadores contables", icon = icon("exclamation-circle"), tabName = "raroc",
                  menuSubItem("Datos", tabName = "RAROC", icon = icon("circle-o")),
                  menuSubItem("Indicadores contables", tabName = "Mor", icon = icon("circle-o"))
                  
                  ),
                  
                  
                  menuItem("Acerca", icon = icon("exclamation-circle"), tabName = "acerca"))
    ),
    dashboardBody(VisionHeader(),
                  
                  tabItems(
                    
                    
                    tabItem(tabName = "subitem1",
                            
                           fluidRow(
                             tabBox( height = "1250px", width = 12,side = "left",
                            
                            tabPanel( title = tagList(shiny::icon("gear"), strong('Scoring')),
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
                            
                           conditionalPanel(condition = "input.userFile == true|| input.dataset == true",
                           fluidRow(
                              box( style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatable'))
                            ))),
                           
                           tabPanel( title = tagList(shiny::icon("gear"), strong('Proyección Scoring')),
                                     
                                     fluidRow(column(6,box(background="yellow",width = 200, checkboxInput("datasetr", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),
                                              column(6,box(background="yellow", width = 200,checkboxInput('userFiler', strong('Cargar Datos Propios'), FALSE)))),
                                     
                                     conditionalPanel(condition = "input.userFiler == true",
                                                      fluidRow(
                                                        box(width = 15, title = h3("Cargar el archivo con los datos"),
                                                            box( width=15,background = "yellow",
                                                                 fileInput('file_dataproy', 'Seleccione el archivo', accept = c('text/csv',
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
                                                                  checkboxInput( width="80%", 'headerproy', "Con encabezado", TRUE)),
                                                              box(width=4,background="yellow",
                                                                  radioButtons( width="40%", 'sepproy', "Separador", c('Coma'=',',
                                                                                                                       'Punto y coma'=';',
                                                                                                                       'Tab'='\t'), ';')),
                                                              box(width=4,background="yellow",
                                                                  radioButtons( width="40%", 'quoteproy', "Comillas", c('Ninguna'='',
                                                                                                                        'Comilla doble'='"',
                                                                                                                        'Comilla simple'="'"), ''))
                                                              
                                                            ))
                                                      )),
                                     
                                     fluidRow(
                                       box(width=12,style = "overflow-x:scroll",status = "warning",dataTableOutput('datatabler'))
                                     )
                                     
                                     
                                     ),
                           tabPanel( title = tagList(shiny::icon("gear"), strong('Rating')),
                                     
                                     fluidRow(
                                       fluidRow(column(6,box(width = 11,background="yellow", checkboxInput("datasetRat", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),
                                                column(6,box(width = 12,background="yellow", checkboxInput('userFileRat', strong('Cargar Datos Propios'), FALSE))))
                                     ),
                                     conditionalPanel(condition = "input.userFileRat == true",
                                                      fluidRow(
                                                        box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                            box( width=15,background = "yellow",
                                                                 fileInput('file_dataRat', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                                           placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                                            ),
                                                            fluidRow(
                                                              box(width=4,background="yellow",strong(ENCABEZADO_TEXT),
                                                                  checkboxInput( width="80%", 'headerRat', WITHHEADER_TEXT, TRUE)),
                                                              box(width=4,background="yellow",
                                                                  radioButtons( width="40%", 'sepRat', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                                              box(width=4,background="yellow",
                                                                  radioButtons( width="40%", 'quoteRat', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                                            )
                                                        )
                                                      )),
                                     conditionalPanel(condition = "input.userFileRat == true|| input.datasetRat == true",
                                                      fluidRow(
                                                        box( style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatableRat'))
                                                      ))
                                     
                                     ),
                           
                           tabPanel( title = tagList(shiny::icon("gear"), strong('Proyección Rating')),
                                     
                                     fluidRow(
                                       fluidRow(column(6,box(width = 11,background="yellow", checkboxInput("datasetRatN", strong("Datos provenientes del Score"), FALSE))),
                                                column(6,box(width = 12,background="yellow", checkboxInput('userFileRatN', strong('Cargar Datos Propios'), FALSE))))
                                     ),
                                     
                                     conditionalPanel(condition = "input.userFileRatN == true",
                                                      fluidRow(
                                                        box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                            box( width=15,background = "yellow",
                                                                 fileInput('file_dataRatN', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                                           placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                                            ),
                                                            fluidRow(
                                                              box(width=4,background="yellow",strong(ENCABEZADO_TEXT),
                                                                  checkboxInput( width="80%", 'headerRatN', WITHHEADER_TEXT, TRUE)),
                                                              box(width=4,background="yellow",
                                                                  radioButtons( width="40%", 'sepRatN', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                                              box(width=4,background="yellow",
                                                                  radioButtons( width="40%", 'quoteRatN', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                                            )
                                                        )
                                                      ))
                                     
                                     
                                     )
                             
                    ))),
                    
                    
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
                                                   conditionalPanel(condition = "(input.selec == 1)", box(title = "Selección de variables cualitativas",width=6,status = "warning",
                                                                                                          height = "140px", numericInput("significancia","Ingrese el nivel de significancia",value = 0.05,min = 0.001,max=1)),
                                                                    box(title = "Region de Rechazo",height = "140px","A continuación se muestra el resultado de una prueba de hipotesis de independencia
                                                                        si el estadístico es mayor que el valor critico podemos suponer la existencia de un efecto entre las variables",status = "warning"),
                                                                    
                                                   box(width = 12,status = "warning",style = "overflow-x:scroll",dataTableOutput('datatablecu'))),
                                                          
                                                   conditionalPanel(condition = "(input.selec1 == 2)",
                                                                    box(style = "overflow-x:scroll",title = "Selección de variables cuantitativas",width=6,status = "warning", 
                                                                        numericInput("significancia1","Ingrese el nivel de significancia",value = 0.05,min = 0.001,max=1)
                                                                                                            ),box(title = "Region de Rechazo",width=6,height = "140px","A continuación se muestra el resultado de una prueba de hipotesis de independencia
                                                                        si el estadístico es mayor que el valor critico podemos suponer la existencia de un efecto entre las variables",status = "warning"),box(width = 12,status = "warning",style = "overflow-x:scroll",dataTableOutput('datatablecu1'))))
                                                  )
                               )
                             )
                    ),
                    
                    tabItem(tabName = "lgd",
                            
                            fluidRow(
                              tabBox(
                                height = "1250px", width = 12,side = "left", 
                                
                                
                                tabPanel( title = tagList(shiny::icon("gear"), strong('Datos')),h3("Pérdidas por incumplimiento"),
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
                                            box(style = "overflow-x:scroll",width=5,status = "warning",dataTableOutput('datatablerl')),
                                            box(width=7,status = "warning", plotlyOutput("curvalgd")))
                                          
                                          
                                ),
                                
                                
                                tabPanel( title = tagList(shiny::icon("gear"), strong('Pérdidas Usando Bootstrap')),h3("Pérdidas por incumplimiento"),
                                          
                                          fluidRow(box(status = "warning",h3("Número de sub-muestras"), numericInput("boot" , label = "",value = 100)),
                                                   
                                                   box(h3("Tamaños de las submuestras en porcentaje"),status = "warning", 
                                                       numericInput("bootT",label = "",value = 20,min = 1,max = 100)) ),
                                          fluidRow(box(width=12,status = "warning", title = h3("Histograma Bootstrap"),plotlyOutput("booot1"))),
                                          fluidRow(box(width=4,status="warning", height = "100px",uiOutput("boots3")),box(width=4,status="warning", height = "100px",numericInput("boot23",label = " Nivel de Confianza",max = 100,min = 1,value = 95)),box(width=4,status="warning", height = "100px",uiOutput("boots4")))
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                )
                                
                                
                                
                                )))
                    
                    ,tabItem(tabName = "glm",
                            
                            fluidRow(
                              tabBox( height = "1250px", width = 12,side = "left",
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Selección y resultados del modelo')),
                                                
                                               
                                                fluidRow(
                                                box(width=12, title =h2("Modelos de probabilidad lineal"),solidHeader = T,status = "warning",radioButtons("radio1", h3("Escoga el link del Modelo"),
                                                                                            choices = list("Modelo Probit" = "probit", "Modelo Logit" = "logit",
                                                                                                           "Modelo Cauchit" = "cauchit"),selected = "probit")),
                                                box(width = 12,title = h1("Coeficientes del Modelo"),status = "warning",dataTableOutput("coefglm")),
                                                box(width = 12,title = h1("Información estadística del Modelo"),status = "warning",dataTableOutput("estglm")),
                                                
                                                box(title = h2("Matriz de confusión"),width=4,status="warning",tableOutput("accur")),box(title = h2("Gráfico ROC"),width=8,status="warning",plotOutput("roc"))
                                                
                                      )),
                                      
                                      
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Score de la cartera de crédito de entrenamiento.')),
                                                
                                                h2("Score y probabilidad de incumplimiento de los clientes"),
                                                dataTableOutput("score"),downloadButton('download',"Descargar datos")
                                               
                                                
                                                
                                                
                                                
                                                
                                      ),
                                      
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Proyección a nuevos clientes')),
                                                
                                                h2("Score y probabilidad de incumplimiento del nuevo cliente")
                                                
                                                
                                                ,
                                               
                                               fluidRow(box(width=12,style = "overflow-x:scroll",status = "warning", title = h2("Proyección"), dataTableOutput("proy"))),downloadButton('download1',"Descargar datos")
                                                
                                                
                                                
                                                
                                      )
                                      
                                      
                                      
                              ))),
                    
                    
                    tabItem(tabName = "rat",
                            
                            fluidRow(
                              tabBox( height = "1250px", width = 12,side = "left",
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Construcción del Modelo de Rating')),
                                                
                                                
                                                
                                                conditionalPanel(condition = "input.userFileRat == true|| input.datasetRat == true",
                                                              fluidRow(
                                                              box( style = "overflow-x:scroll",width=12,title = h3("Información del modelo basado en análisis de disciminante"),status = "warning",dataTableOutput('datatableRatInf'))
                                                             ))
                                      
                                      
                                      
                              ),
                              
                              tabPanel( title = tagList(shiny::icon("gear"), strong('Rating de nuevos clientes.'))
                                        
                                        ,
                                        conditionalPanel(condition = "input.userFileRatN == true|| input.datasetRatN == true",
                                                         fluidRow(
                                                           box( style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatableRatNCF'))
                                                         ),downloadButton('download2',"Descargar datos"))
                                        
                                        
                                        
                                        
                                        
                              )
                              
                              
                              )))
                    
                    
                    
                    
                    ,
                    
                    tabItem( tabName = "datini",
                             fluidRow(
                               tabBox( height = "1250px", width = 12,side = "left",
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Exposición'))
                                                 
                                                 
                                                 ,
                                                 
                                                 fluidRow(
                                                   box(width = 15, title = h3("Cargar el archivo con las exposiciines crediticias"),
                                                       box( width=15,background = "yellow",
                                                            fileInput('file_dataEXP', 'Seleccione el archivo', accept = c('text/csv',
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
                                                             checkboxInput( width="80%", 'headerEXP', "Con encabezado", TRUE)),
                                                         box(width=4,background="yellow",
                                                             radioButtons( width="40%", 'sepEXP', "Separador", c('Coma'=',',
                                                                                                                 'Punto y coma'=';',
                                                                                                                 'Tab'='\t'), ';')),
                                                         box(width=4,background="yellow",
                                                             radioButtons( width="40%", 'quoteEXP', "Comillas", c('Ninguna'='',
                                                                                                                  'Comilla doble'='"',
                                                                                                                  'Comilla simple'="'"), ''))
                                                       )
                                                   )
                                                 ),
                                                 
                                                 fluidRow(
                                                   box(width=12,status = "warning",dataTableOutput('datatableEXP'))
                                                 )
                                                 
                                                 
                                                 ),
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Probabilidad de Incumplimiento')),
                                                 
                                            
                                                 fluidRow(
                                                   fluidRow(column(6,box(width = 12,background="yellow", checkboxInput("datasetPro", strong("Provenientes del Score"), FALSE))),
                                                            column(6,box(width = 12,background="yellow", checkboxInput('userFilePro', strong('Cargar Datos Propios'), FALSE))))
                                                   ),
                                                 conditionalPanel(condition = "input.userFilePro == true",
                                                                  fluidRow(
                                                                    box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                                        box( width=15,background = "yellow",
                                                                             fileInput('file_dataPro', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                                                       placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                                                        ),
                                                                        fluidRow(
                                                                          box(width=4,background="yellow",strong(ENCABEZADO_TEXT),
                                                                              checkboxInput( width="80%", 'headerPro', WITHHEADER_TEXT, TRUE)),
                                                                          box(width=4,background="yellow",
                                                                              radioButtons( width="40%", 'sepPro', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                                                          box(width=4,background="yellow",
                                                                              radioButtons( width="40%", 'quotePro', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                                                        )
                                                                    )
                                                                  )),
                                                 
                                                 conditionalPanel(condition = "input.userFilePro == true|| input.datasetPro == true",
                                                                  fluidRow(
                                                                    box( style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatablePro'))
                                                                  ))  
                                                 
                                                 
                                                 
                                                 
                                       ),
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Pérdidas Dado el Incumplimiento')),
                                                 
                                                 fluidRow(
                                                   fluidRow(
                                                            column(6,box(width = 12,background="yellow", checkboxInput('PerdiGene', strong('Pérdidas Por clientes'), FALSE))),
                                                            column(6,box(width = 12,background="yellow", checkboxInput('userFilePerd', strong('Cargar Datos Propios'), FALSE))))
                                                 ),
                                                 
                                                
                                                 
                                                 
                                                 conditionalPanel(condition = " input.PerdiGene && !input.userFilePerd",
                                                                  fluidRow(
                                                                    box( style = "overflow-x:scroll",width=12,status = "warning",numericInput("PerEsp", 
                                                                                                                                              h3("Pérdida Esperada en porcentaje"), 
                                                                                                                                              value = 75,min = 0.0001,max = 99.99))
                                                                  )),
                                                 conditionalPanel(condition = " !input.PerdiGene && input.userFilePerd",
                                                                  
                                                                  fluidRow(
                                                                    box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                                        box( width=15,background = "yellow",
                                                                             fileInput('file_dataPer', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                                                       placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                                                        ),
                                                                        fluidRow(
                                                                          box(width=4,background="yellow",strong(ENCABEZADO_TEXT),
                                                                              checkboxInput( width="80%", 'headerPer', WITHHEADER_TEXT, TRUE)),
                                                                          box(width=4,background="yellow",
                                                                              radioButtons( width="40%", 'sepPer', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                                                          box(width=4,background="yellow",
                                                                              radioButtons( width="40%", 'quotePer', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                                                        )
                                                                    )
                                                                  ),
                                                                  fluidRow(
                                                                    box( style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('PerdidaPropia'))
                                                                  ))
                                                 
                                                 
                                                 
                                                 
                                                 
                                       )
                                       
                                       
                                       
                                       
                                       
                                       ))),
                                                 
                                                 
                                                 
                    
                    
                    tabItem( tabName = "Param",
                             fluidRow(
                               tabBox( height = "1250px", width = 12,side = "left",
                    
                                                
                     
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Pérdida esperada por cliente')) ,  
                                                 fluidRow(box(width = 12, background="yellow",status = "warning", numericInput("uniper","Ingrese unidad de pérdida",value = 100))),
                                                 fluidRow(box(width = 12,title = "Pérdida esperada por cliente",status = "warning",dataTableOutput("perclien")))
                                                
                                                 
                                                 
                                                 
                                                 
                                       ),
                                       
                                       
                            
                            tabPanel( title = tagList(shiny::icon("gear"), strong('Incumplimientos')),
                                      h3("Modelo CreditRisk+"),
                                      fluidRow(box(style = "overflow-x:scroll",width = 12,title = "Probabilidades incumplimiento",status = "warning",dataTableOutput("numincum"))),
                                      fluidRow(box(width = 12,title = "Distribución Acumulada de Número de Incumplimientos",status = "warning",plotlyOutput("comparacion1"))),
                                      
                                      fluidRow(column(6,box(width = 200, background="yellow",status = "warning", numericInput("uni","Ingrese porcentaje de recuperación luego del default",value = 80)))),
                                     
                                      
                                      
                                      
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
                                                         ),
                                      fluidRow(
                                        box(width=12,title="Exposicion pon bandas en unidades de perdia",status = "warning",dataTableOutput('bandas'))
                                      )
                                      
                                      
                                      
                                      ),
                            
                            tabPanel( title = tagList(shiny::icon("gear"), strong('Pérdida')),
                                      
                                      
                                      fluidRow(
                                        box(width=12,status = "warning",dataTableOutput('Perd23'))
                                      ),
                                      
                                      
                                      fluidRow(box(width = 12,title = "Distribución Acumulada de Pérdidas (En Unidades)",status = "warning",plotlyOutput("comparacion2")))
                                      
                                      
                                     
                                      
                                      
                            ),
                            
                            
                            
                            
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
                             )
                             
                    ), tabPanel( title = tagList(shiny::icon("gear"), strong('Promedio de Pérdidas')), 
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
                                   box(width=12,status = "warning",dataTableOutput('datatablecrm1')))),
                    
                    
                    tabPanel( title = tagList(shiny::icon("gear"), strong('Pérdidas Usando Bootstrap')),
                              fluidRow(box(width = 4,status = "warning",h3("Número de sub-muestras"),height = "160px", numericInput("bootC" , label = "",value = 100)),
                                       
                                       box(width = 4,h3("Tamaños de las submuestras"),status = "warning", height = "160px",
                                           numericInput("bootTC",label = "",value = 20,min = 1,max = 100)) ,box(width = 4,h3("Nivel de Confianza"),height = "160px",status = "warning",label = " Nivel de Confianza",numericInput("boot2312",label = " ",max = 100,min = 1,value = 95))),
                              
                              fluidRow(
                                box(width=12,status = "warning",dataTableOutput('datatablecrm2')))
                              
                              
                            
                              
                              
                              )
                    ))
                    
                    
                    
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
                                       
                                   
                    tabPanel(title = tagList(shiny::icon("gear"), strong('Datos y parámetros')),
                          fluidRow(  
                            fluidRow(box(width=12,title = h3("Metodología"),solidHeader = T,status ="warning" ,checkboxGroupInput("meto",h3(""), 
                                                                                               choices = list("CreditRisk+" = 1, 
                                                                                                              "Credimetrics" = 2)))),
                            
                          
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
                            fluidRow(column(4,box(width=12,status = "warning",solidHeader = T,title = h3("RAROC"),h3(textOutput("raro")))),column(4,box(width=12,solidHeader = T,status = "warning",title = h3("RORAC"),h3(textOutput("roracc")))),column(4, box(width=12,solidHeader = T,status = "warning",title = h3("RARORAC"),h3(textOutput("raroracc"))))),
                            fluidRow(column(4,box(width=12,solidHeader = T,status = "warning",title = h3("Indice de Morósidad"),h3(textOutput("morosidad")))),column(4,box(width=12,solidHeader = T,status = "warning",title = h3("Indice de Cobertura"),h3(textOutput("cobertura")))),column(4, box(width=12,solidHeader = T,status = "warning",title = h3("RAR"),h3(textOutput("rar")))))
                              
                            
                            

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
