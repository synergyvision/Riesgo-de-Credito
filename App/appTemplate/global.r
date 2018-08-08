rm(list = ls())

library(shiny)
library(shinydashboard)
library(plyr)
library(MASS)
library(caret)
library(e1071)
library(pROC)


# Encabezado Vision
VisionHeader <- function(){tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
  tags$img(src="img/vision1.png" , id = "VisionLogo", width = 130 ),
  singleton(includeScript("www/js/d3.js")),
  singleton(includeScript("www/js/underscore.js")),
  singleton(includeScript("www/js/jquery-ui.js")),
  singleton(includeCSS("www/css/app.css"))
)}

ACERTITLE_TEXT<-"Acerca de VisionRisk™"
ACERSUBSV_TEXT<-"Tecnología para Especulación, Inversión, Economía, Finanzas y Riesgo"
ACERVER_TEXT<-"Versión: 0.0.0"
ACERRIF_TEXT<-"Rif: "
ACERRS_TEXT<-"Copyright © 2014-2018 Synergy Vision"
ACERRS_TEXT2 <- "All Rights Reserved"
ACERDIR_TEXT<-"Centro Gerencial Mohedano, La Castellana"
ACERTLF_TEXT<-"0212-2630808 / 0414-2769752"
ACERCORR_TEXT<-"contacto@synergy.vision"

####Data de Ejemplo

mydata <- read.csv("C:/Users/arturo/Documents/Riesgo-de-Credito/App/appTemplate/data/datos_completos.csv")
attach(mydata)

############################################# DATA TEXTO###############################################

UPLOADDATA_TEXT<-"Cargar el archivo con los datos"
SELECTFILE_TEXT<-'Seleccione el archivo'
FILESELEC_TEXT<-'Aun no seleccionas el archivo...'
BUTTSELEC_TEXT<-'Buscar'
WITHHEADER_TEXT<-"Con encabezado"
SEPARATOR_TEXT<-"Separador"
COMILLAS_TEXT<-"Comillas"
ENCABEZADO_TEXT<-"Encabezado de los datos"


UPLOADFILETYPE_CONF<-c('text/csv',
                       'text/comma-separated-values',
                       'text/tab-separated-values',
                       'text/plain',
                       '.csv',
                       '.tsv',
                       '.rda')

UPLOADFILESEP_CONF<-c('Coma'=',',
                      'Punto y coma'=';',
                      'Tab'='\t')

UPLOADCOMILLAS_CONF<-c('Ninguna'='',
                       'Comilla doble'='"',
                       'Comilla simple'="'")
