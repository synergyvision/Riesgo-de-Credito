source("text.R")
source("conf.R")

header<-dashboardHeader( title = tags$img(src="img/BV.png", width=80, height=60) )

menu<-dashboardSidebar(
  sidebarMenu(
    shinythemes::themeSelector(),
    id= "menuitems",
    h3(APPTITLE_TEXT,style="text-align:center;"),
    menuItem(DATAMENUTITLE_TEXT, tabName="data",icon=icon("fal fa-database"), selected = TRUE),
    menuItem(STATMENUTITLE_TEXT, tabName="stat", icon=icon("fas fas fa-table")),
    menuItem(ANALMENUTITLE_TEXT, tabName="variables", icon=icon("fas fa-columns")),
    menuItem(ACERTITLE_TEXT, tabName="acerca", icon=icon("fas fa-code"))
  )
)

datatab<-tabItem(
  tabName="data",
  fluidRow(box(background="red", checkboxInput("dataset", strong("Selecciona para inciar Datos de Ejemplo"), FALSE))),
  fluidRow(box(background="red", checkboxInput('userFile', strong('Cargar Datos Propios'), FALSE))),
  fluidRow(box(background="red", selectInput('selected'))),
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
)

statab<-tabItem( tabName = "stat",
                  fluidRow(
                    tabBox( height = "1250px", width = 12,side = "left",
                            tabPanel( title = tagList(shiny::icon("gear"), strong(RENDTITLE_TEXT)),
                                      box( width=12,status = "success",
                                           dataTableOutput('rendimientos')
                                      )
                            ),
                            tabPanel( title = tagList(shiny::icon("gear"), strong(FRETITLE_TEXT)),
                                      box( width=12,status = "success",
                                           dataTableOutput('frecuencia')
                                      )
                            ),
                            tabPanel( title = tagList(shiny::icon("gear"), strong(STATTITLE_TEXT)),
                                      box( width=12,status = "success",
                                           dataTableOutput('estadisticas1')
                                      ),
                                      box( width=12,status = "success",
                                           dataTableOutput('estadisticas2')
                                      )
                            )
                    )
                  )
)

analitab<-tabItem( tabName = "variables",
                   fluidRow(
                     tabBox( height = "1250px", width = 12,side = "left",
                             tabPanel( title = tagList(shiny::icon("gear"), strong(AHORROTABTITLE_TEXT)),
                                       tags$hr(),
                                       h2(PRUEBDISTTITLE_TEXT),
                                       fluidRow(
                                         box( width=10, status = "success",
                                              tableOutput("matrix1")
                                         )
                                       ),
                                       tags$hr(),
                                       h2(DISTWITHPARAMTITLE_TEXT),
                                       fluidRow(
                                         box( width=6,background = "navy",
                                              selectInput( width="100%", inputId = "distsA", label = SELECFUNCTION_TEXT,
                                                           choices= DISTANALAH_CONF, selected = NULL)
                                         )
                                       ),
                                       fluidRow( uiOutput("paramA") ),
                                       tags$hr(),
                                       h2(GRAFTITLE_TEXT),
                                       fluidRow(
                                         column(6, box( width = 12, status="success", highcharter::highchartOutput("hist1") )
                                         ),
                                         column(6, box( width = 12, status="success", highcharter::highchartOutput("histfreq1") )
                                         )
                                       ),
                                       fluidRow(
                                         column(6, box( width = 12, status="success", highcharter::highchartOutput("plot1") )
                                         ),
                                         column(6, box( width = 12, status="success", plotOutput("qqnorm1") )
                                         )
                                       )
                             ),
                             tabPanel(title = tagList(shiny::icon("gear"), strong(CORRITABTITLE_TEXT)),
                                      tags$hr(),
                                      h2(PRUEBDISTTITLE_TEXT),
                                      fluidRow(
                                        box( width=10, status = "success",
                                             tableOutput("matrix2")
                                        )
                                      ),
                                      tags$hr(),
                                      h2(DISTWITHPARAMTITLE_TEXT),
                                      fluidRow(
                                        box( width=6,background = "navy",
                                             selectInput( width="100%", inputId = "distsC", label = SELECFUNCTION_TEXT,
                                                          choices= DISTANALAH_CONF, selected = NULL)
                                        )
                                      ),
                                      fluidRow( uiOutput("paramC") ),
                                      tags$hr(),
                                      h2(GRAFTITLE_TEXT),
                                      fluidRow(
                                        column(6, box( width = 12, status="success", highcharter::highchartOutput("hist2") ) ),
                                        column(6, box( width = 12, status="success", highcharter::highchartOutput("histfreq2") ) )
                                      ),
                                      fluidRow(
                                        column(6, box( width = 12, status="success", highcharter::highchartOutput("plot2") ) ),
                                        column(6, box( width = 12, status="success", plotOutput("qqnorm2") ) )
                                      )
                             ),
                             tabPanel(title = tagList(shiny::icon("gear"), strong(CORRIRTABTITLE_TEXT)),
                                      tags$hr(),
                                      h2(PRUEBDISTTITLE_TEXT),
                                      fluidRow(
                                        box( width=10, status = "success",
                                             tableOutput("matrix3")
                                        )
                                        
                                      ),
                                      tags$hr(),
                                      h2(DISTWITHPARAMTITLE_TEXT),
                                      fluidRow(
                                        box( width=6,background = "navy",
                                             selectInput( width="100%", inputId = "distsCR", label = SELECFUNCTION_TEXT,
                                                          choices= DISTANALAH_CONF, selected = NULL)
                                        )
                                      ),
                                      fluidRow( uiOutput("paramCR") ),
                                      tags$hr(),
                                      h2(GRAFTITLE_TEXT),
                                      fluidRow(
                                        column(6, box( width = 12, status="success", highcharter::highchartOutput("hist3") ) ),
                                        column(6, box( width = 12, status="success", highcharter::highchartOutput("histfreq3") ) )
                                      ),
                                      fluidRow(
                                        column(6, box( width = 12, status="success", highcharter::highchartOutput("plot3") ) ),
                                        column(6, box( width = 12, status="success", plotOutput("qqnorm3") ) )
                                      )
                             )
                     )
                   )
)

acerca<-tabItem(
  tabName = "acer",
  fluidRow(
    uiOutput("acerca")
  )
)

body<-dashboardBody(
  tabItems( datatab, statab, analitab, acerca)
)

dashboardPage(skin = "black", header, menu, body)