
rm(list = ls())


library(fastDummies)
library(readr)
library(shiny)
library(shinydashboard)
library(latex2exp)
library(grid)
library(gridExtra)
library(plyr)
library(MASS)
library(caret)
library(e1071)
library(pROC)
library(plotly)
library(plyr)
library(DT)

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
ACERVER_TEXT<-"Versión: 1.0.0"
ACERRIF_TEXT<-"Rif: "
ACERRS_TEXT<-"Copyright © 2014-2018 Synergy Vision"
ACERRS_TEXT2 <- "All Rights Reserved"
ACERDIR_TEXT<-"Centro San Ignacio, La Castellana"
ACERTLF_TEXT<-"0212-2630808 / 0414-2769752"
ACERCORR_TEXT<-"contacto@synergy.vision"

####Data de Ejemplo


clases1 <- read.csv("data/Historica_de_perdidas.csv",sep = ";")
clases2 <- read.csv("data/clases.csv",sep = ";")
lgd <- read.csv("data/loss_given.csv",sep = ";")
transic <- read.csv("data/Historica_de_migraciones.csv",sep = ";")
rat <- read.csv("data/rat.csv",sep=";")
perdidas <-  read.csv("data/perdidas.csv",sep=";")
mydata <- read.csv("data/mydata.csv",sep=";")

reg <- read.csv("data/reg.csv")

creditos <- read.csv("data/creditos.csv")
attach(mydata)

############################################# DATA TEXTO###############################################

UPLOADDATA_TEXT<-"Cargar el Archivo con los Datos"
SELECTFILE_TEXT<-'Seleccione el Archivo'
FILESELEC_TEXT<-'Aún no Seleccionas el Archivo...'
BUTTSELEC_TEXT<-'Buscar'
WITHHEADER_TEXT<-"Con Encabezado"
SEPARATOR_TEXT<-"Separador"
COMILLAS_TEXT<-"Comillas"
ENCABEZADO_TEXT<-"Encabezado de los Datos"


UPLOADFILETYPE_CONF<-c('text/csv',
                       'text/comma-separated-values',
                       'text/tab-separated-values',
                       'text/plain',
                       '.csv',
                       '.tsv',
                       '.rda')

UPLOADFILESEP_CONF<-c('Coma'=',',
                      'Punto y Coma'=';',
                      'Tab'='\t')

UPLOADCOMILLAS_CONF<-c('Ninguna'='',
                       'Comilla Doble'='"',
                       'Comilla Simple'="'")
