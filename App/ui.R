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
                           menuSubItem("Scoring y Rating", tabName = "subitem1", icon = icon("circle-o")),
                           menuSubItem("Pérdida por Incumplimiento", tabName = "subitem2", icon = icon("circle-o")),
                           menuSubItem("Matriz de Transición", tabName = "subitem3", icon = icon("circle-o")),
                           menuSubItem("CreditRisk+", tabName = "datini", icon = icon("circle-o")),
                           menuSubItem("CreditMetrics", tabName = "CRED", icon = icon("circle-o")),
                           menuSubItem("Backtesting", tabName = "datos_back", icon = icon("folder-open")),
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
                  
                  
                  menuItem("Creditmetrics", icon = icon("th"), tabName = "crm",
                          
                           menuSubItem("Simulación y Resultados", tabName = "RES", icon = icon("circle-o")),
                           menuSubItem("Stress Testing", tabName = "ST2", icon = icon("circle-o"))
                           
                  ),
                  menuItem("Backtesting", icon = icon("angle-double-left"), 
                           
                           menuSubItem("Resultados", tabName = "resultados_back", icon = icon("file-alt"))
                  ),
                  
                  menuItem("Indicadores Contables", icon = icon("exclamation-circle"), tabName = "raroc",
                  menuSubItem("Indicadores Contables", tabName = "Mor", icon = icon("circle-o"))
                  
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
                             fluidRow(column(6,box(width = 11,background="yellow", checkboxInput("dataset", strong("Datos de Ejemplo"), FALSE))),column(6,box(width = 12,background="yellow", checkboxInput('userFile', strong('Datos Propios'), FALSE)))),
                            fluidRow(
                              column(12,box( background="yellow",width=12,status = "warning",
                                   selectInput('columns', 'Selección de Variable de Estudio', "Seleccione los Datos")
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
                                     
                                     fluidRow(column(6,box(background="yellow",width = 200, checkboxInput("datasetr", strong("Datos de Ejemplo"), FALSE))),
                                              column(6,box(background="yellow", width = 200,checkboxInput('userFiler', strong('Datos Propios'), FALSE)))),
                                     
                                     conditionalPanel(condition = "input.userFiler == true",
                                                      fluidRow(
                                                        box(width = 15, title = h3("Cargar el Archivo con los Datos"),
                                                            box( width=15,background = "yellow",
                                                                 fileInput('file_dataproy', 'Seleccione el archivo', accept = c('text/csv',
                                                                                                                                'text/comma-separated-values',
                                                                                                                                'text/tab-separated-values',
                                                                                                                                'text/plain',
                                                                                                                                '.csv',
                                                                                                                                '.tsv',
                                                                                                                                '.rda'),
                                                                           placeholder = 'Aún no Seleccionas el Archivo...', buttonLabel = 'Buscar' )
                                                            ),
                                                            fluidRow(
                                                              box(width=4,background="yellow",strong("Encabezado de los Datos"),
                                                                  checkboxInput( width="80%", 'headerproy', "Con Encabezado", TRUE)),
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
                                       fluidRow(column(6,box(width = 11,background="yellow", checkboxInput("datasetRat", strong("Datos de Ejemplo"), FALSE))),
                                                column(6,box(width = 12,background="yellow", checkboxInput('userFileRat', strong('Datos Propios'), FALSE))))
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
                                       fluidRow(column(6,box(width = 11,background="yellow", checkboxInput("datasetRatN", strong("Datos Provenientes del Score"), FALSE))),
                                                column(6,box(width = 12,background="yellow", checkboxInput('userFileRatN', strong('Datos Propios'), FALSE))))
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
                                                      )),conditionalPanel(condition = "input.datasetRatN == true|| input.userFileRatN == true",
                                                                          fluidRow(
                                                                            box( style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatableRatinf'))
                                                                          ))
                                     
                                     
                                     )
                             
                    ))),
                    
                    
                    tabItem( tabName = "subitem3",
                             fluidRow(
                               tabBox( height = "1250px", width = 12,side = "left",
                                       
                                tabPanel( title = tagList(shiny::icon("gear"), strong('Migraciones Históricas ')),
                                          fluidRow(column(6,box(background="yellow",width = 200, checkboxInput("datasetMT", strong("Datos de Ejemplo"), FALSE))),column(6,box(background="yellow", width = 200,checkboxInput('userFileMT', strong('Datos Propios'), FALSE)))),
                                          
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
                                         fluidRow( box(style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatableMT')))
                                          
                                          
                                          
                                          )))),
                    
                    
                    
                    
                    tabItem( tabName = "subitem2",
                             fluidRow(
                               tabBox( height = "1250px", width = 12,side = "left",
                                       
                                       
                                       
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Pérdidas por Clientes')),
                                                 
                                                 fluidRow(column(6,box(background="yellow",width = 112, checkboxInput("datasetrl", strong("Datos de Ejemplo"), FALSE))), column(6,box(background="yellow",width = 112, checkboxInput('userFilerl', strong('Datos Propios'), FALSE)))),
                                                 
                                                 conditionalPanel(condition = "input.userFilerl == true",
                                                                  fluidRow(
                                                                    box(width = 15, title = h3("Cargar el Archivo con los Datos"),
                                                                        box( width=15,background = "yellow",
                                                                             fileInput('file_datarl', 'Seleccione el Archivo', accept = c('text/csv',
                                                                                                                                          'text/comma-separated-values',
                                                                                                                                          'text/tab-separated-values',
                                                                                                                                          'text/plain',
                                                                                                                                          '.csv',
                                                                                                                                          '.tsv',
                                                                                                                                          '.rda'),
                                                                                       placeholder = 'Aún no Seleccionas el Archivo...', buttonLabel = 'Buscar' )
                                                                        ),
                                                                        fluidRow(
                                                                          box(width=4,background="yellow",strong("Encabezado de los Datos"),
                                                                              checkboxInput( width="80%", 'headerrl', "Con Encabezado", TRUE)),
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
                                                   box(style = "overflow-x:scroll",width = 12,status = "warning",dataTableOutput('datatablerl'))
                                                 )
                                                 
                                                 ),
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Pérdida por Clases')),
                                                 
                                                 
                                                 fluidRow(column(6,box(background="yellow", width = 120,checkboxInput("datasetC", strong("Datos de Ejemplo"), FALSE))),column(6,box(background="yellow",width = 120, checkboxInput('userFileC', strong('Datos Propios'), FALSE)))),
                                                 
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
                                                 
                                       )
                                       
                                       
                                       ))),
                    
                    
                    
                    tabItem( tabName = "CRED",
                             fluidRow(
                               tabBox( height = "1250px", width = 12,side = "left",
                                       
                                       
                                       
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Expocisión')),
                                                 
                                                 
                                                 fluidRow(column(6,box(background="yellow",width = 200, checkboxInput("dataset0", strong("Datos de Ejemplo"), FALSE))),column(6,box(background="yellow", width = 200,checkboxInput('userFile0', strong('Datos Propios'), FALSE)))),
                                                 
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
                                                                                                placeholder = 'Aún no Seleccionas el Archivo...', buttonLabel = 'Buscar' )
                                                                        ),
                                                                        
                                                                        box(width=4,background="yellow",strong("Encabezado de los Datos"),
                                                                            checkboxInput( width="80%", 'headecrm0', "Con Encabezado", TRUE)),
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
                                                 
                                                 
                                       )
                                       ,
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Matriz de Transición')),
                                                 
                                                 box(width = 120, h2("Matriz de Probabilidades de Transición")),
                                                 fluidRow( column(width=6,box(background="yellow",width = 200, checkboxInput("datasetcrm", strong("Matriz de Transición Calculada"), FALSE))),column(width=6,box(background="yellow",width = 200, checkboxInput('userFilecrm', strong("Matriz de Transición Propia"), FALSE)))),
                                                 conditionalPanel(condition = "input.userFilecrm == true",
                                                                  
                                                                  box(width = 15, title = h3("Cargar el Archivo con la Matriz de Transición"),
                                                                      fluidRow( box( width=12,background = "yellow",
                                                                                     fileInput('file_datacrm', 'Seleccione el archivo', accept = c('text/csv',
                                                                                                                                                   'text/comma-separated-values',
                                                                                                                                                   'text/tab-separated-values',
                                                                                                                                                   'text/plain',
                                                                                                                                                   '.csv',
                                                                                                                                                   '.tsv',
                                                                                                                                                   '.rda'),
                                                                                               placeholder = 'Aún no Seleccionas el Archivo...', buttonLabel = 'Buscar' )
                                                                      ),
                                                                      
                                                                      box(width=4,background="yellow",strong("Encabezado de los Datos"),
                                                                          checkboxInput( width="80%", 'headecrm', "Con Encabezado", TRUE)),
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
                                                 
                                                 
                                                 
                                       ),
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Pérdida por Clase')),
                                                 
                                                 fluidRow(
                                                   fluidRow(column(6,box(width = 11,background="yellow", checkboxInput("dataset_Cla_Cr", strong("Pérdida por Clase Calculada"), FALSE))),column(6,box(width = 12,background="yellow", checkboxInput('userFile_Cla_Cr', strong('Datos Propios'), FALSE))))
                                                   ),
                                                 conditionalPanel(condition = "input.userFile_Cla_Cr == true",
                                                                  fluidRow(
                                                                    box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                                        box( width=15,background = "yellow",
                                                                             fileInput('file_data_Cla_Cr', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                                                       placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                                                        ),
                                                                        fluidRow(
                                                                          box(width=4,background="yellow",strong(ENCABEZADO_TEXT),
                                                                              checkboxInput( width="80%", 'header_Cla_Cr', WITHHEADER_TEXT, TRUE)),
                                                                          box(width=4,background="yellow",
                                                                              radioButtons( width="40%", 'sep_Cla_Cr', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                                                          box(width=4,background="yellow",
                                                                              radioButtons( width="40%", 'quote_Cla_Cr', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                                                        )
                                                                    )
                                                                  )),
                                                 
                                                 conditionalPanel(condition = "input.userFile_Cla_Cr == true|| input.dataset_Cla_Cr == true",
                                                                  fluidRow(
                                                                    box( style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatable_Cla_Cr'))
                                                                  ))
                                                 
                                                 
                                                 
                                                 )
                                       
                                      
                               )
                               )
                    ),
                    
                    tabItem( tabName = "stat",
                             fluidRow(
                               tabBox( height = "1250px", width = 12,side = "left",
                                       
                                       
                                       
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Relación de las Variables Independientes')),
                                                 fluidRow(
                                                   box( background="yellow",width=12,status = "warning",
                                                        selectInput('columns1', 'Selección de Variable de Estudio', "Seleccione los Datos"),actionButton("goButton", "Actualizar Variable")
                                                   )
                                                   
                                                 ),
                                                 
                                                 
                                                 
                                                 fluidRow(column(width=11,plotlyOutput("comparacion")),
                                                          box( width=12,title = "Resumen Estadístico de la Variable Seleccionada",status = "warning",
                                                                                           dataTableOutput('estad1')
                                                 ))
                                                 
                                                 
                                                 
                                                 
                                                 
                                                 
                                       ),
                                       
                                       
                                       
                                       
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong("Selección de Variables")),
                                                 fluidRow(
                                                   
                                                   box(width=12,status = "warning",checkboxGroupInput("selec", 
                                                                                                      h3("Tipo de Selección"), 
                                                                                                      choices = list("Selección de Variables Cualitativas" = 1)),checkboxGroupInput("selec1" ,
                                                                                                                                                                                     h3(""), 
                                                                                                                                                                                     choices = list(
                                                                                                                                                                                                    "Selección de Variables Cuantitativas" = 2))),
                                                   conditionalPanel(condition = "(input.selec == 1)", box(title = "Selección de Variables Cualitativas",width=6,status = "warning",
                                                                                                          height = "180px", numericInput("significancia","Ingrese el Nivel de Significancia",value = 0.05,min = 0.001,max=1),actionButton("goButton1", "Actualizar Nivel de Significancia")),
                                                                    box(title = "Region de Rechazo",height = "180px","A continuación se muestra el resultado de una prueba de hipotesis de independencia
                                                                        si el estadístico es mayor que el valor critico podemos suponer la existencia de un efecto entre las variables",status = "warning"),
                                                                    
                                                   box(width = 12,status = "warning",style = "overflow-x:scroll",dataTableOutput('datatablecu'))),
                                                          
                                                   conditionalPanel(condition = "(input.selec1 == 2)",
                                                                    box(style = "overflow-x:scroll",title = "Selección de variables cuantitativas",width=6,status = "warning", 
                                                                        numericInput("significancia1","Ingrese el Nivel de Significancia",value = 0.05,min = 0.001,max=1),actionButton("goButton2", "Actualizar Nivel de Significancia")
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
                                
                                
                                tabPanel( title = tagList(shiny::icon("gear"), strong('Datos')),
                                          
                                          
                                          fluidRow(
                                            
                                            box(title = h2("Histograma de Pérdidas"),width=12,status = "warning", plotlyOutput("curvalgd")))
                                          
                                          
                                ),
                                
                                
                                tabPanel( title = tagList(shiny::icon("gear"), strong('Pérdidas Usando Bootstrap')),
                                          
                                          fluidRow(box(height = "200px",status = "warning",h3("Número de Sub-Muestras"), numericInput("boot" , label = "",value = 100), actionButton("goButton5", "Actualizar")),
                                                   
                                                   box(height = "200px",h3("Tamaños de las Submuestras en Porcentaje"),status = "warning", 
                                                       numericInput("bootT",label = "",value = 20,min = 1,max = 100), actionButton("goButton6", "Actualizar") ) ),
                                          fluidRow(box(width=12,status = "warning", title = h3("Histograma Bootstrap"),plotlyOutput("booot1"))),
                                          fluidRow(box(width=4,status="warning", height = "100px",uiOutput("boots3")),box(width=4,status="warning", height = "100px",numericInput("boot23",label = " Nivel de Confianza",max = 100,min = 1,value = 95)),box(width=4,status="warning", height = "100px",uiOutput("boots4")))
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                )
                                
                                
                                
                                )))
                    
                    ,tabItem(tabName = "glm",
                            
                            fluidRow(
                              tabBox( height = "1250px", width = 12,side = "left",
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Selección y Resultados del Modelo')),
                                                
                                               
                                                fluidRow(
                                                box(width=12, title =h2("Modelos de Probabilidad Lineal"),solidHeader = T,status = "warning",radioButtons("radio1", h3("Escoga el Link del Modelo"),
                                                                                            choices = list("Modelo Probit" = "probit", "Modelo Logit" = "logit",
                                                                                                           "Modelo Cauchit" = "cauchit"),selected = "probit"),actionButton("goButton3", "Actualizar Link")),
                                                box(width = 12,title = h1("Coeficientes del Modelo"),status = "warning",dataTableOutput("coefglm")),
                                                box(width = 12,title = h1("Información Estadística del Modelo"),status = "warning",dataTableOutput("estglm")),
                                                
                                                box(title = h2("Matriz de Confusión"),width=4,status="warning",tableOutput("accur")),box(title = h2("Gráfico ROC"),width=8,status="warning",plotOutput("roc"))
                                                
                                      )),
                                      
                                      
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Score de la Cartera de Crédito de Entrenamiento.')),
                                                
                                                h2("Score y Probabilidad de Incumplimiento de los Clientes"),
                                                dataTableOutput("score"),downloadButton('download',"Descargar Datos")
                                               
                                                
                                                
                                                
                                                
                                                
                                      ),
                                      
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Proyección a Nuevos Clientes')),
                                                
                                                h2("Score y Probabilidad de Incumplimiento del Nuevo Cliente")
                                                
                                                
                                                ,
                                               
                                               fluidRow(box(width=12,style = "overflow-x:scroll",status = "warning", title = h2("Proyección"), dataTableOutput("proy"))),downloadButton('download1',"Descargar Datos")
                                                
                                                
                                                
                                                
                                      )
                                      
                                      
                                      
                              ))),
                    
                    
                    tabItem(tabName = "rat",
                            
                            fluidRow(
                              tabBox( height = "1250px", width = 12,side = "left",
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Construcción del Modelo de Rating')),
                                                
                                                
                                                
                                                conditionalPanel(condition = "input.userFileRat == true|| input.datasetRat == true",
                                                              fluidRow(
                                                              box( style = "overflow-x:scroll",width=12,title = h3("Información del Modelo Basado en Análisis de Disciminante"),status = "warning",dataTableOutput('datatableRatInf'))
                                                             ))
                                      
                                      
                                      
                              ),
                              
                              tabPanel( title = tagList(shiny::icon("gear"), strong('Rating de Nuevos Clientes.'))
                                        
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
                                                                      placeholder = 'Aún no Seleccionas el Archivo...', buttonLabel = 'Buscar' )
                                                       ),
                                                       fluidRow(
                                                         box(width=4,background="yellow",strong("Encabezado de los Datos"),
                                                             checkboxInput( width="80%", 'headerEXP', "Con Encabezado", TRUE)),
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
                                                   fluidRow(column(6,box(width = 12,background="yellow", checkboxInput('userFilePro', strong('Datos Propios'), FALSE))),
                                                            column(6,box(width = 12,background="yellow", checkboxInput("datasetPro", strong("Provenientes del Score"), FALSE))) )
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
                                                            column(6,box(width = 12,background="yellow", checkboxInput('userFilePerd', strong('Datos Propios'), FALSE))))
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
                    
                                                
                     
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Pérdida Esperada por Cliente')) ,  
                                                 fluidRow(box(width = 12, background="yellow",status = "warning", numericInput("uniper","Ingrese Unidad de Pérdida",value = 100))),
                                                 fluidRow(box(width = 12,title = "Pérdida Esperada por Cliente",status = "warning",dataTableOutput("perclien")))
                                                
                                                 
                                                 
                                                 
                                                 
                                       ),
                                       
                                       
                            
                            tabPanel( title = tagList(shiny::icon("gear"), strong('Incumplimientos')),
                                      h3("Modelo CreditRisk+"),
                                      fluidRow(box(style = "overflow-x:scroll",width = 12,title = "Probabilidades Incumplimiento",status = "warning",dataTableOutput("numincum"))),
                                      fluidRow(box(width = 12,title = "Distribución Acumulada de Número de Incumplimientos",status = "warning",plotlyOutput("comparacion1")))
                                      

                                      
                                      
                                      
                                      
                                     
                                      
                                      
                                      
                                      ),
                            
                            tabPanel( title = tagList(shiny::icon("gear"), strong('Pérdida')),
                                      
                                      
                                      fluidRow(
                                        box(width=12,status = "warning",dataTableOutput('Perd23'))
                                      ),
                                      
                                      
                                      fluidRow(box(width = 12,title = "Distribución Acumulada de Pérdidas (En Unidades)",status = "warning",plotlyOutput("comparacion2")))
                                      
                                      
                                     
                                      
                                      
                            ),
                            
                            
                            
                            
                            tabPanel( title = tagList(shiny::icon("gear"), strong('Resultados')),
                                     
                                       fluidRow(box(title =h3("Escoga Nivel de Confianza Para el VaR"),solidHeader = T,width=12,status = "warning",radioButtons("conf", "",
                                                                                                                choices = list("90%" = 90, "95%" = 95,
                                                                                                                               "99%" = 99),selected = 95), actionButton("goButtonvar", "Calcular")),   
                                               
                                               
                                               fluidRow(column(4,box(width=12,status = "warning",h3("Pérdida Esperada"), h2(textOutput("pe")) )),column( 4,box(width=12,status = "warning",h3("Valor en Riesgo"), h2(textOutput("var"))  )),column(4,box(width=12,status = "warning",h3("TVaR"), h2(textOutput("tvar"))))),
                                               fluidRow( column( 12,box(width=12,h2("Reporte"), status = "warning",downloadButton("reporte1","Descargar")))))
                                      
                                      
                                      
                                                          )))),
                    tabItem(tabName = "ST1",
                            
                            fluidRow(column(6,box(height = "300px",width=12,title = h2("StressTesting"),solidHeader = T,status = "warning",radioButtons("estres2", h3("Nivel de Estrés de la Prueba"),
                                                                                                                              choices = list("1 %" = 0.01, "5 %" = 0.05,
                                                                                                                                             "10 %" = 0.1),selected = 0.01),actionButton("goButtonstre", "Calcular"))),column(6,box(height = "300px",width=12,title = h2("Resultado"),solidHeader = T,status = "warning",h3("El Valor de la Prueba"),h1(textOutput("Stress")))))
                            
                            
                    )
                    
                    
                    
                    
                   ,   
                   
                   
                    tabItem( tabName = "CMT",fluidRow(
                      tabBox( height = "1250px", width = 12,side = "left",
                              
                              
                              
                              
                              
                              tabPanel( title = tagList(shiny::icon("gear"), 
                                                        
                      strong('Cálculo de la Matriz de Transición')),
                    
                      
                    
                          
                            fluidRow(
                              
                              box(title = h3("Matriz de Transición Calculada"),style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatableMTR'))
                            )
                      
                      
                           
                              
                            
                    )
                    
                    ))),
                    
                    
                    
                    
                    
                    tabItem( tabName = "CPC", fluidRow(
                      tabBox( height = "1250px", width = 12,side = "left"
                              
                              
                              
                              
                              
                              , tabPanel( title = tagList(shiny::icon("gear"), strong('Pérdidas Promedios')), 
                                 
                                 
                                 fluidRow(
                                   box(width=12,title = h1("Pérdida Esperada por Clase"),status = "warning",dataTableOutput('datatablecrm1')))),
                    
                    
                    tabPanel( title = tagList(shiny::icon("gear"), strong('Pérdidas Usando Bootstrap')),
                              fluidRow(box(width = 4,status = "warning",h3("Número de Sub-Muestras"),height = "160px", numericInput("bootC" , label = "",value = 100)),
                                       
                                       box(width = 4,h3("Tamaños de las submuestras"),status = "warning", height = "160px",
                                           numericInput("bootTC",label = "",value = 20,min = 1,max = 100)) ,
                                       box(width = 4,h3("Nivel de Confianza"),height = "160px",status = "warning",label = " Nivel de Confianza",
                                           numericInput("boot2312",label = " ",max = 100,min = 1,value = 95))),
                              
                              fluidRow(
                                box(width=12,status = "warning",dataTableOutput('datatablecrm2')))
                              
                              
                            
                              
                              
                              )
                    ))
                    
                    
                    
                    ),
                    
                    tabItem( tabName = "RES" , 
                             
                             fluidRow(
                               
                               box(title = h3("Escoga Nivel de Confianza Para el VaR"),width=12,solidHeader = T,status = "warning",radioButtons("conf1","", 
                                                                                                                                                choices = list("90%" = 90, "95%" = 95,
                                                                                                                                                               "99%" = 99),selected = 95)),
                             box(width=8, status = "warning",numericInput("simcrm","Número de Simulaciones",value = 2),actionButton("goButtonSim", "Iniciar Simulación") ),
                            
                             
                             
                             box(width=12,status = "warning", plotlyOutput("credime")),
                             fluidRow(column(4, box(title = h3("Pérdida Esperada:"),width=12,status = "warning", h3(textOutput("pe122")) )),
                                      column(4,box(title = h3("Valor en Riesgo:"),width=12,status = "warning", h3(textOutput("var122")) )),
                                      column(4, box(title = h3("TVaR:"),width=12,status = "warning",h3(textOutput("tvar122")) 
                                                  ))),downloadButton("reporte2","Descargar")
                             
                             
                            
                             
                            )
                             
                             
                             
                             
                    ),
                   
                   tabItem(tabName = "ST2",
                           
                           fluidRow(column(6,box(width=12,title = h2("StressTesting"),solidHeader = T,status = "warning",radioButtons("stress3", h3("Nivel de Estrés de la Prueba"),
                                                                                  choices = list("1 %" = 0.01, "5 %" = 0.05,
                                                                                                 "10 %" = 0.1),selected = 0.1))),column(6,box(width=12,title = h2("Resultado"),solidHeader = T,status = "warning",h3("El Valor de la Prueba"),h1(textOutput("Stres45")))))
                           
                           
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
                    
                   #BACKTESTING
                   tabItem(tabName = "datos_back",
                           h2(" Seleccionar archivo"),
                           fluidRow(
                             box(width = 12, title = h3(UPLOADDATA_TEXT),
                                 box( width=12,background = "yellow",
                                      fileInput('file_data_back', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                 ),
                                 fluidRow(
                                   box(width=4,background="yellow",strong(ENCABEZADO_TEXT),
                                       checkboxInput( width="100%", 'header_back', WITHHEADER_TEXT, TRUE)),
                                   box(width=4,background="yellow",
                                       radioButtons( width="40%", 'sep_back', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                   box(width=4,background="yellow",
                                       radioButtons( width="40%", 'quote_back', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                 )
                             )
                           ),
                           fluidRow(
                             box(width=12,style="overflow-x:scroll",status = "warning",dataTableOutput('datatable_back'))
                           )
                           
                           
                   ),#final tabitem Backtesting
                   tabItem(tabName = "resultados_back",
                           h3(" Elegir porcentaje del Backtesting:"),
                           box(width = 12, background = "yellow",
                               selectInput( inputId = "porback", "Seleccione Porcentaje del VaR", choices = c(.90, .95, .99), selected = .95)
                           ),
                           verbatimTextOutput("back_porcentaje"),
                           h2("Resultados"),
                           fluidRow(
                             box(width=12,style="overflow-x:scroll",status = "warning",verbatimTextOutput('result_back'))
                           ),
                           h2(" Valores críticos"),
                           plotlyOutput("grafico_back"),
                           h2("Reporte"),
                           downloadButton("report_back", "Descargar")
                           
                           
                   ),
                   
                   
                   tabItem( tabName = "Mor" ,
                            fluidRow(column(4,box(width=12,status = "warning",solidHeader = T,title = h3("RAROC"),h3(textOutput("raroc1")))),column(4,box(width=12,solidHeader = T,status = "warning",title = h3("RORAC"),h3(textOutput("roracc")))),column(4, box(width=12,solidHeader = T,status = "warning",title = h3("RARORAC"),h3(textOutput("raroracc"))))),
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
