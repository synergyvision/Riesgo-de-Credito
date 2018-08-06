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
                  
        menuItem("Datos", tabName = "data", icon = icon("fal fa-database")),
        menuItem("Seleccion de variables", tabName="stat", icon=icon("fas fas fa-table")),
        menuItem("Modelo de probabiidad lineal", icon = icon("th"), tabName = "glm"),
        menuItem("Acerca", icon = icon("exclamation-circle"), tabName = "acerca"))
  ),
    dashboardBody(VisionHeader(),

      tabItems(

        tabItem(tabName = "data",
                
                
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
                  box(width=12,status = "danger",dataTableOutput('datatable'))
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
                                     box( background="red",width=12,status = "success",
                                          textOutput('variables1')
                                     ),
                                     
                                     box( background="red",width=12,status = "success",
                                          numericInput("num1", 
                                                       h3("Seleccione la variable a comparar"), 
                                                       value = 1)
                                     ),
                                     
                                     plotOutput("comparacion")
                                     
                                     
                           ),
                           
                           
                           
                           
                           
                           tabPanel( title = tagList(shiny::icon("gear"), strong('Estadísticos Básicos')),
                                     
                                     box( background="red",width=12,status = "success",
                                          textOutput('varia23')
                                     ),
                                     
                                     box( width=12,status = "success",
                                          dataTableOutput('estad1')
                                     )
                                     
                           )
                   )
                 )
        ),
        tabItem(tabName = "glm",
                
                h2("Modelos de probabilidad lineal"),
               box(width=12, background="red",radioButtons("radio", h3("Escoga el link del Modelo"),
                            choices = list("Modelo Probit" = "probit", "Modelo Logit" = "logit",
                                           "Modelo Cauchit" = "cauchit"),selected = "probit"))
                
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

