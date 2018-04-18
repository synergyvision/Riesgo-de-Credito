library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(rriskDistributions)
library(DT)
library(VaRES)
library(rmarkdown)
library(dygraphs)
library(readr)
library(webshot)
library(rintrojs)
library(highcharter)
library(CASdatasets)

source("text.R")
source("conf.R")
data("credit")


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

#Selección análisis cuentas de ahorro
DISTANALAH_CONF<-c("Normal"="Normal", "Exponential"="Exponential",
                   "Cauchy"="Cauchy", "Logistic"="Logistic",
                   "Beta"="Beta", "Chi-square"="Chi-square",
                   "Uniform"="Uniform","Gamma"="Gamma",
                   "Lognormal"="Lognormal", "Weibull"="Weibull",
                   "F"="F", "Student"="Student", "Gompertz"="Gompertz")
