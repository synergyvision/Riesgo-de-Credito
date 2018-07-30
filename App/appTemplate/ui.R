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
                  
        menuItem("Gráficos", icon = icon("bar-chart-o"),
                 
          menuSubItem("Sub-item 1", tabName = "subitem1", icon = icon("circle-o")),
          
          menuSubItem("Sub-item 2", tabName = "subitem2", icon = icon("circle-o"))
        ),
        
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        
        menuItem("Indicadores", icon = icon("th"), tabName = "widgets"),
        menuItem("Acerca", icon = icon("exclamation-circle"), tabName = "acerca"))
  ),
    dashboardBody(VisionHeader(),

      tabItems(

        tabItem(tabName = "dashboard",
                
          fluidRow(
            
            box(plotOutput("plot1", height = 250),status = "warning"),
            
            box(
              title = "Controls",
              sliderInput("slider", "Number of observations:", 1, 100, 50)
            )
          )
        ),

        tabItem(tabName = "widgets",
                
                h2("Ingrese códigos para generar indicadores")
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

