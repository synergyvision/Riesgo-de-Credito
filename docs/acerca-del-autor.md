--- 
title: "Riesgo de Crédito"
subtitle: "Ciencia de los Datos Financieros"
author: "Synergy Vision"
date: "2019-05-06"
knit: "bookdown::render_book"
documentclass: krantz
bibliography: [book.bib, packages.bib]
biblio-style: apalike
link-citations: yes
colorlinks: yes
lot: yes
lof: yes
fontsize: 12pt
monofontoptions: "Scale=0.8"
keep_md: yes
site: bookdown::bookdown_site
description: ""
url: 'http\://synergy.vision/Riesgo-de-Credito/'
github-repo: synergyvision/Riesgo-de-Credito/
cover-image: images/cover.png
---

# Prefacio {-}

Placeholder


## ¿Por qué  leer este libro? {-}
## Estructura del libro {-}
## Información sobre los programas y convenciones {-}
## Agradecimientos {-}

<!--chapter:end:index.Rmd-->


# Acerca del Autor {-}

Este material es un esfuerzo de equipo en Synergy Vision, (<http://synergy.vision/nosotros/>). En especial al equipo conformado por Danny Morales y Arturo Carreño. Los cuales en conjunto y con su experiencia en el sector financiero crearon este sistema para la facilitar a las instituciones cumplir con las regulaciones obligatorias exigidas por el ente regulatorio.

Danny Morales: Licenciado en ciencian actuariales, cuenta con varios años de experiencia en el sector financiero y en la actualidad es gerente general de synergy vision y profesor universitario (UCV) 

Arturo Carreño: Liceciado en matemáticas, cuenta con experiencia en el sector financiero y en la actualidad es cientifico de datos financieros de synergy vision y profesor universitario (UCV)

Nuestra cuenta de Twitter es (https://twitter.com/bysynergyvision) y nuestros repositorios están en GitHub (https://github.com/synergyvision).
  		  
 **Somos Científicos de Datos Financieros**

<!--chapter:end:000-author.Rmd-->


# Introducción 

Placeholder



<!--chapter:end:010-introduction.Rmd-->


# Introducción a la teoría de probabilidad

Placeholder


## Espacio de probabilidad
## Variables aleatorias y función de distribución.
###  Variable aleatoria
### Tipos de variables aleatorias
### Distribución de probabilidad:
#### Distribución de probabilidad discreta
#### Distribución de probabilidad continua
### Función de distribución: 
#### Caso discreto
#### Caso continuo
### Algunos tipos de variables aleatorias
#### Bernoulli
#### Binomial
#### Poisson
#### Distribución uniforme
#### Distribución exponencial
#### Distribución normal
### Esperanza de una variable aleatoria.
#### Esperanza de una variable aleatoria discreta.
#### Esperanza de una variable aleatoria continua.
### Varianza de una variable aleatoria.
### Covarianza de variables aleatorias.

<!--chapter:end:100-ConceptosMate.Rmd-->


# Conceptos de la gestión del riesgo de crédito

Placeholder


## Pérdida esperada
## Probabilidad de incumplimiento (DP)
## Exposición ($EAD$)
## Pérdida dado el incumplimiento ($LGD$)
## Pérdida inesperada ($UL$)
## Capital económico o valor en riesgo
##  Matriz de transición

<!--chapter:end:200-nociones.Rmd-->


# Scoring de credito

Placeholder


## Estructura del Scoring de Crédito
### Los modelos probit y logit.
### Interpretación del modelo
### Relación entre el score y el riesgo
### Uso de modelos lineales para la selección de variables (Métodos de búsqueda secuencial).
#### Estimación por etapas (paso a paso o stepwise)
#### La adición progresiva (forward) y la eliminación regresiva (backward)
#### Advertencias sobre los métodos de búsqueda secuenciales

<!--chapter:end:300-Scoring.Rmd-->


# CreditRisk+

Placeholder


## La distribución del número de incumplimientos con probabilidades de impago fijas.
## La agrupación por bandas de exposición las pérdidas
## La determinación de la *distribución de pérdidas* de la cartera.
## Obteniendo la distribución de las pérdidas
## Obtención del VaR

<!--chapter:end:301-creditrisk+.Rmd-->


# CreditMetrics

Placeholder


## Elementos que usa **Credimetrics**
## El caso de un solo crédito 
### Ejemplo 1
### Ejemplo 2
## Cartera de dos de créditos con probabilidades de transición independientes.
### Ejemplo 3
## Aproximación de la distribución de pérdidas de una cartera con $n$ créditos.
### Simulación de MonteCarlo
#### ¿Que es un Método de Montecarlo?
#### Ejemplo: Gotas de lluvia para estimar $\pi$
#### Números pseudoaleatorios
#### Aplicando simulación de MonteCarlo para hallar la distribución de pérdidas de una cartera.
#### Obtención de las métricas de riesgo.

<!--chapter:end:302-creditmetrics.Rmd-->


# Manual técnico de la aplicación Vision CreditRisk

Placeholder


## Presentación de la aplicación.
## Primera sección: CreditRisk+
### Datos
### Estadísticos:
#### Relación de las variables independientes
### Selección de variables
## Pérdida por incumplimiento
## Score de Crédito.
### Selección y resultados del modelo
### Score de la cartera de crédito.
### Proyección a nuevos clientes.
## Parametros y resultados.
### Parámetros iniciales
### Resultados
## Stress Testing
## Segunda sección: Credimetrics
## Créditos
## Matriz de trancición
### Cálculo de la matriz de transición
### Selecección de la matriz de transición
## Pérdida por clase.
### Cálculo de pérdida esperada
### Pérdida esperada
##  Simulación y resultados.
## Stress Testing.
## Tercera sección: Indicadores contables
## Cálculo del RAROC
### RAROC

<!--chapter:end:303-ManualVisionRisk.Rmd-->


# (APPENDIX) Apéndice {-}

Placeholder

# Software Tools

Placeholder


## R and R packages
## Pandoc
## LaTeX

<!--chapter:end:400-apendice.Rmd-->

# Referencias {-}




<!--chapter:end:500-references.Rmd-->

