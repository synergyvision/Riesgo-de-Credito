
#App title
APPTITLE_TEXT<-"VisionScore™"

#Menu titles
DATAMENUTITLE_TEXT<-"Datos"
STATMENUTITLE_TEXT<-"Estadística"
ANALMENUTITLE_TEXT<-"Selección de Variables"
VARMENUTITLE_TEXT<-"Valor en Riesgo"
GARCHMENUTITLE_TEXT <- "Modelo GARCH"
GAPMENUTITLE_TEXT<-"GAP de liquidez"
INFMENUTITLE_TEXT<-"Informe"
ACERMENUTITLE_TEXT<-"Acerca de"

############################################# DATA ###############################################

UPLOADDATA_TEXT<-"Cargar el archivo con los datos"
SELECTFILE_TEXT<-'Seleccione el archivo'
FILESELEC_TEXT<-'Aun no seleccionas el archivo...'
BUTTSELEC_TEXT<-'Buscar'
WITHHEADER_TEXT<-"Con encabezado"
SEPARATOR_TEXT<-"Separador"
COMILLAS_TEXT<-"Comillas"
ENCABEZADO_TEXT<-"Encabezado de los datos"

############################################ ESTADÍSTICA ##############################################

RENDTITLE_TEXT<-'Rendimientos'
FRETITLE_TEXT<-'Frecuencias'
STATTITLE_TEXT<-'Estadísticos Básicos'

############################################# ANÁLISIS ##############################################

PRUEBDISTTITLE_TEXT<-"Resultados de Pruebas de Distribución"
DISTWITHPARAMTITLE_TEXT<-"Función de Distribución con los Parámetros correspondientes"
GRAFTITLE_TEXT<-"Gráficos"
AHORROTABTITLE_TEXT<-"Análisis Cuentas de Ahorro"
CORRITABTITLE_TEXT<-"Análisis Cuentas Corrientes"
CORRIRTABTITLE_TEXT<-"Análisis Cuentas Corrientes Remuneradas"
SELECFUNCTION_TEXT<-"Seleccione una Distribución"
CORRTABTITLE_TEXT<-"Análisis Cuentas Corrientes"
CORRRTABTITLE_TEXT<-"Análisis Cuentas Corrientes Remuneradas"

#SELECCIÓN DE DISTRIBUCIÓN
#NORMAL
NLABEL1<-"Media"
NLABEL2<-"Desviación Típica"
#EXPONENCIAL
ELABEL1<-"Lambda"
#CAUCHY
CLABEL1<-"Mu"
CLABEL2<-"Theta"
#LOGISTICA
LLABEL1<-"S"
LLABEL2<-"L"
#BETA
BLABEL1<-"S1"
BLABEL2<-"S2"
#CHICUADRADO
CCLABEL<-"Grados de Libertad"
#UNIFORME
ULABEL1<-"Valor mínimo"
ULABEL2<-"Valor máximo"
#GAMMA
GLABEL1<-"M"
GLABEL2<-"Lambda"
#LOGNORMAL
LNLABEL1<-"Media"
LNLABEL2<-"Desviación Típica"
#WEIBULL
WLABEL1<-"S1"
WLABEL2<-"S2"
#FISHER
FLABEL1<-"Grados de Libertad 1"
FLABEL2<-"Grados de Libertad 2"
#T-STUDENT
TLABEL1<-"Grados de Libertad"
#GOMPERTZ
GOLABEL1<-"S1"
GOLABEL2<-"S2"

#GRÁFICO DE HISTOGRAMA
##TITULO
HISTTITLE_TEXT<- "Histograma de los Rendimientos"
HISTATITLE_TEXT<- "Cuenta de Ahorro"
HISTCTITLE_TEXT<- "Cuenta Corriente"
HISTCRTITLE_TEXT<- "Cuenta Corriente Remunerada"
##NOMBRE DE LOS EJES
HISTEJEX_TEXT<-"Valores"
HISTEJEY_TEXT<- "Densidad"

#GRÁFICO DE VALORES EN SERIE
##TITULO
VALSERATITLE_TEXT<- "Valores en Serie Cuentas de Ahorro"
VALSERCTITLE_TEXT<- "Valores en Serie Cuentas Corriente"
VALSERCRTITLE_TEXT<- "Valores en Serie Cuentas Corriente Remunerada"
##LABEL
VALSERLABEL_TEX<-"Saldo"

#GRÁFICO DE RENDIMIENTOS
##TITULO
RENDATITLE_TEXT<- "Rendimientos Cuentas de Ahorro"
RENDCTITLE_TEXT<- "Rendimientos Cuentas Corriente"
RENDCRTITLE_TEXT<- "Rendimientos Cuentas Corriente Remunerada"
##LABEL
RENDLABEL_TEX<- "% Volatilidad"

#GRÁFICO QQNORM
##TITULO
QQNORMATITLE_TEXT<- "Grafico Q-Q Cuentas de Ahorro"
QQNORMCTITLE_TEXT<- "Grafico Q-Q Cuentas Corriente"
QQNORMCRTITLE_TEXT<- "Grafico Q-Q Cuentas Corriente Remunerada"
##EJES
QQNORMEJEX_TEXT<- "Cuantiles Teóricos"
QQNORMEJEY_TEXT<- "Cuantiles Muestrales"

############################################# VAR ###############################################
#TÍTULO DE LA CAJA
BOXSELECVARTITLE_TEXT<-"Seleccione Porcentaje del VaR"

#SUBTITULOS DEL ITEM
VARAHTITLE_TEXT<-"Valor en Riesgo (VaR) para las Cuentas de Ahorro"
VARCOTITLE_TEXT<-"Valor en Riesgo (VaR) para las Cuentas Corrientes"
VARCRTITLE_TEXT<-"Valor en Riesgo (VaR) para las Cuentas Corrientes Remuneradas"

#DISTRIBUCIÓN
VARINNOR_TEXT<-"Formulación del VaR para la Distribución Normal $$VaR_p(X) = \\mu + \\sigma \\Phi^{-1}(p)$$"
VARINEXP_TEXT<-"Formulación del VaR para la Distribución Exponencial $$VaR_p(X) = -\\frac{1}{\\lambda}log(1-p)$$"
VARINCAU_TEXT<-"Formulación del VaR para la Distribución Cauchy $$VaR_p(X) = \\mu + \\sigma tan(\\pi(p-\\frac{1}{2}))$$"
VARINLOG_TEXT<-"Formulación del VaR para la Distribución Logistica $$VaR_p(X) = \\mu + \\sigma log[p(1-p)]$$"
VARINBET_TEXT<- "Formulación del VaR para la Distribución Beta"
VARINCHC_TEXT<- "Formulación del VaR para la Distribución Chi Cuadrado"
VARINUNF_TEXT<- "Formulación del VaR para la Distribución Uniforme $$VaR_p(X) = a + p(b-a)$$"
VARINGAM_TEXT<- "Formulación del VaR para la Distribución Gamma $$VaR_p(X) = \\frac{1}{b} Q^{-1}(a, 1-p)$$"
VARINLGN_TEXT<- "Formulación del VaR para la Distribución Lognormal $$VaR_p(X) = e^{[\\mu + \\sigma \\Phi^{-1}(p)]}$$"
VARINWEI_TEXT<- "Formulación del VaR para la Distribución Weibull $$VaR_p(X) = \\sigma[-log(1-p)]^{\\frac{1}{\alpha}}$$"
VARINF_TEXT<- "Formulación del VaR para la Distribución F $$VaR_p(X) = \\mu + F^{-1}(p)\\sigma$$"
VARINTST_TEXT<- "Formulación del VaR para la Distribución T student $$VaR_p(X) = \\mu + T^{-1}(p)\\sigma$$"
VARINGOM_TEXT<- "Formulación del VaR para la Distribución Gompertz"

VARTINNOR_TEXT<- "Formulación del TVaR para la Distribución Normal $$TVaR_p(X) = \\mu + \\frac{σ}{p} \\int_{0}^{p} \\Phi^{-1}(v) dv$$"
VARTINEXP_TEXT<- "Formulación del TVaR para la Distribución Exponencial $$VaR_p(X) = -\\frac{1}{\\lambda}log(1-p)$$"
VARTINCAU_TEXT<- "Formulación del TVaR para la Distribución Cauchy $$TVaR_p(X) = \\mu + \\frac{\\sigma}{p} \\int_{0}^{p} tan(\\pi(v-\\frac{1}{2}))dv$$"
VARTINLOG_TEXT<- "Formulación del TVaR para la Distribución Lognormal $$TVaR_p(X) = \\frac{e^{\\mu}}{p} \\int_{0}^{p} e^{\\sigma \\Phi^{-1}(v)}dv$$"
VARTINBET_TEXT<- "Formulación del TVaR para la Distribución Beta"
VARTINCHC_TEXT<- "Formulación del TVaR para la Distribución Chi Cuadrado"
VARTINUNF_TEXT<- "Formulación del TVaR para la Distribución Uniforme $$TVaR_p(X) = a + \\frac{p}{2} (b-a)$$"
VARTINGAM_TEXT<- "Formulación del TVaR para la Distribución Gamma $$TVaR_p(X) = \\frac{1}{bp} \\int_{0}^{p}Q^{-1}(a, 1-v)dv$$"
VARTINLGN_TEXT<- "Formulación del TVaR para la Distribución Lognormal $$TVaR_p(X) = \\frac{e^{\\mu}}{p} \\int_{0}^{p} e^{\\sigma \\Phi^{-1}(v)}dv$$"
VARTINWEI_TEXT<- "Formulación del TVaR para la Distribución Weibull $$TVaR_p(X) = \\frac{\\sigma}{p}\\gamma[1+\\frac{1}{\\alpha}, -log(1-p)]$$"
VARTINF_TEXT<- "Formulación del TVaR para la Distribución F $$TVaR_p(X) = \\mu + \\int_{0}^{p}F^{-1}(v)\\sigma dv$$"
VARTINTST_TEXT<- "Formulación del TVaR para la Distribución T student $$TVaR_p(X) = \\mu + \\int_{0}^{p}T^{-1}(v)\\sigma dv$$"
VARTINGOM_TEXT<- "Formulación del TVaR para la Distribución Gompertz"

############################################# GAP ###############################################
SELECTITLEGAP_TEXT<- "Seleccione el tipo de Cuenta"

TABLETITLEA_TEXT<- "AHORRO"
TABLETITLEC_TEXT<- "CORRIENTE"
TABLETITLECR_TEXT<- "CORRIENTE REMUNERADA"

########################################### GARCH ###############################################
SELECTITLEGARCH_TEXT <- "Selecione un modelo de serie de tiempo:"






########################################### INFORME #############################################

REVITITLE_TEXT<- "Revisado por"
REALTITLE_TEXT<- "Realizado por"

########################################### ACERCA DE ###########################################

ACERTITLE_TEXT<-"Acerca de VisionScore"
ACERSUBSV_TEXT<-"Tecnología para Especulación, Inversión, Economía, Finanzas y Riesgo"
ACERVER_TEXT<-"Versión: "
ACERRIF_TEXT<-"Rif: "
ACERRS_TEXT<-"Copyright © 2014-2017 Synergy Vision. All Rights Reserved"
ACERDIR_TEXT<-"Centro Gerencial Mohedano, La Castellana"
ACERTLF_TEXT<-"0212-2630808 / 0414-2769752"
ACERCORR_TEXT<-"contacto@synergy.vision"

####################################### USUARIO Y CLAVE ########################################
UCTITLE_TEXT<-"Riesgo de Liquidez"
USUAR_TEXT<-"Usuario:"
CLAVE_TEX<-"Clave:"


