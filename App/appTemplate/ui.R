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
                  
                  menuItem("CreditRisk+", tabName = "data", icon = icon("fal fa-database"),
                           
                           
                           
                           menuSubItem("Datos", tabName = "subitem1", icon = icon("circle-o")),
                           menuSubItem("Estadisticos", tabName = "stat", icon = icon("circle-o")),
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
                    
                    tabItem(tabName = "subitem1",
                            
                            
                            fluidRow(box(background="red", checkboxInput("dataset", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),
                            fluidRow(box(background="red", checkboxInput('userFile', strong('Cargar Datos Propios'), FALSE))),
                            conditionalPanel(condition = "input.userFile == true",
                                             fluidRow(
                                               box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                   box( width=12,background = "red",
                                                        fileInput('file_data', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                                  placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                                   ),
                                                   fluidRow(
                                                     box(width=4,background="red",strong(ENCABEZADO_TEXT),
                                                         checkboxInput( width="80%", 'header', WITHHEADER_TEXT, TRUE)),
                                                     box(width=4,background="red",
                                                         radioButtons( width="40%", 'sep', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                                     box(width=4,background="red",
                                                         radioButtons( width="40%", 'quote', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                                   )
                                               )
                                             )),
                            fluidRow(
                              box(style = "overflow-x:scroll",width=12,status = "danger",dataTableOutput('datatable'))
                            )
                    ),
                    tabItem( tabName = "stat",
                             fluidRow(
                               tabBox( height = "1250px", width = 12,side = "left",
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Variable de estudio'))
                                                 ,box( background="red",width=12,status = "success",
                                                       textOutput('variables')
                                                 )   ,
                                                 box( background="red",width=12,status = "success",
                                                      numericInput("num", 
                                                                   h3("Seleccione la posicion de la variable de estudio"), 
                                                                   value = 1)
                                                 )  
                                                 
                                                 
                                                 
                                                 
                                       ),
                                       
                                       
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Relacion de las variables independientes')),
                                                 fluidRow(box( background="red",width=12,status = "success",
                                                               textOutput('variables1')
                                                 ),
                                                 
                                                 box( background="red",width=12,status = "success",
                                                      numericInput("num1", 
                                                                   h3("Seleccione la variable a comparar"), 
                                                                   value = 2)
                                                 ))
                                                 
                                                 ,
                                                 fluidRow(plotlyOutput("comparacion"))
                                                 
                                                 
                                                 
                                       ),
                                       
                                       
                                       
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Estadísticos Básicos')),
                                                 
                                                 box( background="red",width=12,status = "success",
                                                      textOutput('varia23')
                                                 ),
                                                 
                                                 box( width=12,status = "success",
                                                      dataTableOutput('estad1')
                                                 )
                                                 
                                       ),
                                       tabPanel( title = tagList(shiny::icon("gear"), strong("Selección de variables cualitativas ")),    
                                                  numericInput("significancia","Ingrese el nivel de significancia",value = 0.05,min = 0.001,max=1),
                                                 box(style = "overflow-x:scroll",width=12,status = "danger", dataTableOutput('datatablecu'))),
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong("Selección de variables cuantitativas ")),    
                                                 numericInput("significancia1","Ingrese el nivel de significancia",value = 0.05,min = 0.001,max=1),
                                                 box(style = "overflow-x:scroll",width=12,status = "danger", dataTableOutput('datatablecu1')))
                               )
                             )
                    ),
                    tabItem(tabName = "glm",
                            
                            fluidRow(
                              tabBox( height = "1250px", width = 12,side = "left",
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Seleccion del modelo')),
                                                
                                                h2("Modelos de probabilidad lineal"),
                                                box(width=12, background="red",radioButtons("radio1", h3("Escoga el link del Modelo"),
                                                                                            choices = list("Modelo Probit" = "probit", "Modelo Logit" = "logit",
                                                                                                           "Modelo Cauchit" = "cauchit"),selected = "probit")),
                                                h3("Convertir perfiles negativos al valor 1 y perfiles positivos al valor 0")
                                                ,box(width=12, background="red",radioButtons("radio2", h3("Escoga una opcion"),
                                                                                             choices = list("Cambiar" = 1, "Mantener por defecto" = 2),selected = 2))
                                      ),
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Resultados del modelo')),
                                                
                                                
                                                
                                                
                                                box(width=5, background="red",tableOutput("accur")
                                                ),
                                                box(width=12, background="red",plotOutput("roc")
                                                )),
                                      
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Score de la cartera de credito')),
                                                
                                                h2("Score y probabilidad de incumplimiento de los clientes"),
                                                dataTableOutput("score")
                                               
                                                
                                                
                                                
                                                
                                                
                                      ),
                                      
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Proyeccion a nuevos clientes')),
                                                
                                                h2("Score y probabilidad de incumplimiento del nuevo cliente"),
                                                
                                                fluidRow(box(background="red", checkboxInput("datasetr", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),
                                                fluidRow(box(background="red", checkboxInput('userFiler', strong('Cargar Datos Propios'), FALSE)))
                                                , conditionalPanel(condition = "input.userFiler == true",
                                                                   fluidRow(
                                                                     box(width = 15, title = h3("Cargar el archivo con los datos"),
                                                                         box( width=12,background = "red",
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
                                                                           box(width=4,background="red",strong("Encabezado de los datos"),
                                                                               checkboxInput( width="80%", 'headerr', "Con encabezado", TRUE)),
                                                                           box(width=4,background="red",
                                                                               radioButtons( width="40%", 'sepr', "Separador", c('Coma'=',',
                                                                                                                                 'Punto y coma'=';',
                                                                                                                                 'Tab'='\t'), ';')),
                                                                           box(width=4,background="red",
                                                                               radioButtons( width="40%", 'quoter', "Comillas", c('Ninguna'='',
                                                                                                                                  'Comilla doble'='"',
                                                                                                                                  'Comilla simple'="'"), ''))
                                                                         )
                                                                     )
                                                                   )),
                                                fluidRow(
                                                  box(width=12,status = "danger",dataTableOutput('datatabler'))
                                                ), box(width=12,status = "danger",tableOutput('dat'))
                                                
                                                
                                                
                                      )
                                      
                                      
                                      
                              ))),
                    
                    tabItem(tabName = "Param",
                            
                            tabPanel( title = tagList(shiny::icon("gear"), strong('Parametros iniciales')),
                                      h3("Modelo CreditRisk+"),
                                      box(width = 4, background="red",status = "danger", numericInput("uniper","Ingrese unidad de perdida",value = 1000)),
                                      box(width = 6, background="red",status = "danger", numericInput("uni","Ingrese porcentaje de recuperacion luego del default",value = 80)),
                                      
                                      box(width = 12, background="red",status = "danger",h2("Score y probabilidad de incumplimiento del nuevo cliente")),
                                      fluidRow( box(background="red", checkboxInput("datasetr1", strong("Probabilidades de incumplimiento provenientes del Score"), FALSE))),
                                      fluidRow(box(background="red", checkboxInput('userFiler1', strong("Probabilidades de incumplimiento propias"), FALSE)))
                                      
                                      , conditionalPanel(condition = "input.userFiler1 == true",
                                                         fluidRow(
                                                           box(width = 15, title = h3("Cargar el archivo con las probabiidades de incumplimiento"),
                                                               box( width=12,background = "red",
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
                                                                 box(width=4,background="red",strong("Encabezado de los datos"),
                                                                     checkboxInput( width="80%", 'headerr1', "Con encabezado", TRUE)),
                                                                 box(width=4,background="red",
                                                                     radioButtons( width="40%", 'sepr1', "Separador", c('Coma'=',',
                                                                                                                        'Punto y coma'=';',
                                                                                                                        'Tab'='\t'), ';')),
                                                                 box(width=4,background="red",
                                                                     radioButtons( width="40%", 'quoter1', "Comillas", c('Ninguna'='',
                                                                                                                         'Comilla doble'='"',
                                                                                                                         'Comilla simple'="'"), ''))
                                                               )
                                                           )
                                                         )),fluidRow(
                                                           box(width=12,status = "danger",dataTableOutput('datatabler1'))
                                                         ))),
                    
                    
                    
                    
                    tabItem( tabName = "Res",
                             box(width=12,status = "danger",background="red",radioButtons("conf", h3("Escoga nivel de confianza para el VaR"),
                                                                                          choices = list("90%" = 90, "95%" = 95,
                                                                                                         "99%" = 99),selected = 95)),     
                             
                             box(width=12,status = "danger",background="red",h2("Perdida esperada"), textOutput("pe")),
                             box(width=12,status = "danger",background="red",h2("Valor en rieso"), textOutput("var")),
                             box(width=12,status = "danger",background="red",h2("TVaR"), textOutput("tvar")),
                             h2("Reporte"),
                             downloadButton("reporte1","Descargar")
                    ),
                    tabItem( tabName = "CRED",
                             h1("Creditos"),
                             
                             fluidRow(box(background="red", checkboxInput("dataset0", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),
                             fluidRow(box(background="red", checkboxInput('userFile0', strong('Cargar Datos Propios'), FALSE))),
                             conditionalPanel(condition = "input.userFile0 == true",
                                              fluidRow(
                                                box(width = 15, title = h3("Cargar el archivo con los creditos"),
                                                    box( width=12,background = "red",
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
                                                      box(width=4,background="red",strong("Encabezado de los datos"),
                                                          checkboxInput( width="80%", 'headecrm0', "Con encabezado", TRUE)),
                                                      box(width=4,background="red",
                                                          radioButtons( width="40%", 'sepcrm0', "Separador", c('Coma'=',',
                                                                                                               'Punto y coma'=';',
                                                                                                               'Tab'='\t'), ';')),
                                                      box(width=4,background="red",
                                                          radioButtons( width="40%", 'quotecrm0', "Comillas", c('Ninguna'='',
                                                                                                                'Comilla doble'='"',
                                                                                                                'Comilla simple'="'"), ''))
                                                    )
                                                )
                                              )
                             ),
                             fluidRow(
                               box(style = "overflow-x:scroll",width=12,status = "danger",dataTableOutput('datatable0'))
                             )
                             
                    ),   
                    tabItem(tabName = "CMT", h1('Calculo de la Matriz de transición'),
                            fluidRow(box(background="red", checkboxInput("datasetMT", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),
                            fluidRow(box(background="red", checkboxInput('userFileMT', strong('Cargar Datos Propios'), FALSE))),
                            conditionalPanel(condition = "input.userFileMT == true",
                                             fluidRow(
                                               box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                   box( width=12,background = "red",
                                                        fileInput('file_dataMT', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                                  placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                                   ),
                                                   fluidRow(
                                                     box(width=4,background="red",strong(ENCABEZADO_TEXT),
                                                         checkboxInput( width="80%", 'headerMT', WITHHEADER_TEXT, TRUE)),
                                                     box(width=4,background="red",
                                                         radioButtons( width="40%", 'sepMT', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                                     box(width=4,background="red",
                                                         radioButtons( width="40%", 'quoteMT', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                                   )
                                               )
                                             )),
                            fluidRow(
                              box(style = "overflow-x:scroll",width=12,status = "danger",dataTableOutput('datatableMT'))
                            ),
                            fluidRow(
                              box(title = h3("Matriz de transicion"),style = "overflow-x:scroll",width=12,status = "danger",dataTableOutput('datatableMTR'))
                            )
                    ),
                    
                    tabItem( tabName="MT", h1('Matriz de transición')
                             ,box(width = 12, background="red",status = "danger",h2("Matriz de probabilidades de transicion de la cartera de clientes")),
                             fluidRow( column(width=5,box(background="red", checkboxInput("datasetcrm", strong("Matriz de transicion calculada"), FALSE))),column(width=5,box(background="red", checkboxInput('userFilecrm', strong("Matriz de transicion propia"), FALSE)))),
                             #fluidRow(box(background="red", checkboxInput('userFilecrm', strong("Matriz de transicion propia"), FALSE))),
                             conditionalPanel(condition = "input.userFilecrm == true",
                                              fluidRow(
                                                box(width = 15, title = h3("Cargar el archivo con la matriz de transicion"),
                                                    box( width=12,background = "red",
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
                                                      box(width=4,background="red",strong("Encabezado de los datos"),
                                                          checkboxInput( width="80%", 'headecrm', "Con encabezado", TRUE)),
                                                      box(width=4,background="red",
                                                          radioButtons( width="40%", 'sepcrm', "Separador", c('Coma'=',',
                                                                                                              'Punto y coma'=';',
                                                                                                              'Tab'='\t'), ';')),
                                                      box(width=4,background="red",
                                                          radioButtons( width="40%", 'quotecrm', "Comillas", c('Ninguna'='',
                                                                                                               'Comilla doble'='"',
                                                                                                               'Comilla simple'="'"), ''))
                                                    )
                                                )
                                              )
                                              
                             ),fluidRow(
                               box(width=12,status = "danger",dataTableOutput('datatablecrm')))
                             
                             
                             
                             
                    ),
                    
                    
                    
                    tabItem( tabName = "CPC", h1('Calculo de las Perdidas esperada por clases'),
                             fluidRow(box(background="red", checkboxInput("datasetC", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),
                             fluidRow(box(background="red", checkboxInput('userFileC', strong('Cargar Datos Propios'), FALSE))),
                             conditionalPanel(condition = "input.userFileC == true",
                                              fluidRow(
                                                box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                    box( width=12,background = "red",
                                                         fileInput('file_dataC', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                                   placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                                    ),
                                                    fluidRow(
                                                      box(width=4,background="red",strong(ENCABEZADO_TEXT),
                                                          checkboxInput( width="80%", 'headerC', WITHHEADER_TEXT, TRUE)),
                                                      box(width=4,background="red",
                                                          radioButtons( width="40%", 'sepC', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                                      box(width=4,background="red",
                                                          radioButtons( width="40%", 'quoteC', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                                    )
                                                )
                                              )),
                             fluidRow(
                               box(style = "overflow-x:scroll",width=12,status = "danger",dataTableOutput('datatableC'))
                             ),
                             fluidRow(
                               box(title = h3("Perdida esperada por clase"),style = "overflow-x:scroll",width=12,status = "danger",dataTableOutput('datatableCR'))
                             )
                    ),   
                    
                    tabItem( tabName ="PC"  , h1('Perdida esperada por clase'),
                             box(width = 15, title = h1("Cargar el archivo con las perdida por clase")),
                             box(background="red",checkboxInput("datasetcrm1", strong("Perdida por clases calculada"), FALSE))
                             ,box(background="red", checkboxInput('userFilecrm1', strong("Ingresar perdida esperada por clase"), F)),
                             conditionalPanel(condition = "input.userFilecrm1 == true",
                                              fluidRow(
                                                
                                                box( width=12,background = "red",
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
                                                  box(width=4,background="red",strong("Encabezado de los datos"),
                                                      checkboxInput( width="80%", 'headecrm1', "Con encabezado", TRUE)),
                                                  box(width=4,background="red",
                                                      radioButtons( width="40%", 'sepcrm1', "Separador", c('Coma'=',',
                                                                                                           'Punto y coma'=';',
                                                                                                           'Tab'='\t'), ';')),
                                                  box(width=4,background="red",
                                                      radioButtons( width="40%", 'quotecrm1', "Comillas", c('Ninguna'='',
                                                                                                            'Comilla doble'='"',
                                                                                                            'Comilla simple'="'"), ''))
                                                )
                                                
                                              )
                                              
                             ),fluidRow(
                               box(width=12,status = "danger",dataTableOutput('datatablecrm1')))),
                    
                    tabItem( tabName = "RES" , h1('Simulación y Resultados'),
                             box(width=4,background="red", numericInput("simcrm","suimulación",value = 2) ),
                             box(width=12,status = "danger",background="red",radioButtons("conf1", h3("Escoga nivel de confianza para el VaR"),
                                                                                          choices = list("90%" = 90, "95%" = 95,
                                                                                                         "99%" = 99),selected = 95)),
                             box(title = h1("La perdida esperada es:"),width=12,status = "danger",background="red", textOutput("pe122") ),
                             box(title = h1("El resultado del VaR es:"),width=12,status = "danger",background="red", textOutput("var122") ,h2("Reporte"),
                                 downloadButton("reporte2","Descargar"))
                             
                             
                             
                             
                    ),
                    
                    tabItem( tabName = "RAROC" , h1('Calculo del RAROC' ),hr(),withMathJax(), "La ecuacion estandar para el calculo del RAROC es: $$\\textrm{RAROC}=\\frac{RAR}{C}$$ Donde: $$RAR=\\textrm{Risk Adjusted Return}$$ $$C=\\textrm{Capital necesario para cumbrir el riesgo}$$"),
                    tabItem(tabName = "VRAROC",
                            box(width=12,background="red",checkboxGroupInput("meto",h3("Metodología"), 
                                                                                               choices = list("CreditRisk+" = 1, 
                                                                                                              "Credimetrics" = 2))),
                            box(width=12,background="red",fileInput("file", h3("Ingrese Balance"),placeholder = 'Aun no seleccionas el archivo...', buttonLabel = 'Buscar')),
                            box(width=12,background="red",h3("El resultado del RAROC es"),textOutput("Raroc1"))
                            
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