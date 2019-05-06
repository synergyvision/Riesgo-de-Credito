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

\mainmatter

# Manual técnico de la aplicación Vision CreditRisk


En el presente capítulo, presentaremos el manual técnico de la aplicación Vision CreditRisk, el cual utiliza las nociones teóricas vistas anteriormente para lograr de una forma agradable y de facil entendimiento permtir que el usuario encargado del área de crédito de su institución maneje de forma precisa y adecuada las métricas de riesgo.  

## Presentación de la aplicación.


La aplicación esta compuesta de tres secciones, la primera referente a la metodología CreditRisk+, la segunda correspondiente a la metodología Credimetrics y la tercera sección referente a indicadores técnicos.

![\label{fig:"sd"}](~/Riesgo_de_Credito/portada.png)



## Primera sección: CreditRisk+

En la primera sección que se observa en la columna izquierda, consta de las siguientes secciones: Datos, Estadísticos, Pérdida por incumplimiento, Score de crédito, parámetros y resultados, stressting.

![\label{fig:"sd"}](~/Riesgo_de_Credito/datos.png)

### Datos

En esta sección se cargan los datos con los que se alimentara la costrucción del score de crédito y parte de la metología, al iniciar veremos la siguiente pantalla:

![\label{fig:"sd"}](~/Riesgo_de_Credito/datos1.png)

Tendremos dos opciones, una a manera de ejemplo para entender como debe ser el formato de los datos, al seleccionarla se nos desplegara la siguiente pantalla. 

![\label{fig:"sd"}](~/Riesgo_de_Credito/datos2.png)

Ahora del lado que debemos seleccionar, se nos desplagara:

![\label{fig:"sd"}](~/Riesgo_de_Credito/datos3.png)

Debemos hacer click en buscar los datos a seleccionar, estos datos deberan estar conformados por la lista de los clientes, similar a los datos de ejemplo, una de la columnas debe tener el nombre de "Creditability" (la cual indica si el cliente esta en mora, valor de $1$ o no valor de $0$)

### Estadísticos:

En esta seccion tendremos dos pestañas:

#### Relación de las variables independientes

En esta pestaña, veremos el gráfico estadístico de boxplot para ver el comportamiento de la variable al segmentarla en deudores o no, ademas en la parte inferior tendremos un resumen estadístico de la variable seleccionada.


![\label{fig:"sd"}](~/Riesgo_de_Credito/esta1.png)

### Selección de variables

En esta sección el usuario podrá colocar el significancia para escoger las variables cualitativas y cuantitativas que se usaran en el modelo, la pestaña se divide en 2, una para cada tipo de variable.

![\label{fig:"sd"}](~/Riesgo_de_Credito/esta2.png)



## Pérdida por incumplimiento

En esta sección podra calcular la pérdida histórica por incumplimiento. El usuario podrá ver graficamente lo que espera recuprar en caso de incumplimiento, mientras transcurre el tiempo.

![\label{fig:"sd"}](~/Riesgo_de_Credito/perdida.png)


## Score de Crédito.

En esta sección se calculara la probabilidad de incumplimiento, score o pontuja crediticio por cliente, y se haran proyecciones a nuevos clientes.

### Selección y resultados del modelo

En esta sección el usuario seleccionara el modelo que mejor se ajuste a los datos, una vez seleccionado se deplegara la matriz con los resultados del modelo y el grafico ROC para mostrar el nivel de acierto del modelo.

![\label{fig:"sd"}](~/Riesgo_de_Credito/score1.png)

### Score de la cartera de crédito.

Una vez selecionado el modelo, en esta pestaña el usuario podra ver la probabilidad de incumplimiento y el score o puntaje crediticio de los clientes.

![\label{fig:"sd"}](~/Riesgo_de_Credito/score2.png)


### Proyección a nuevos clientes.



## Parametros y resultados.

En esta sección se pediran los valores de los parámetros básicos y se mostraran los resultados de las principales métricas de riesgo para la metodología CreditRisk+.

![\label{fig:"sd"}](~/Riesgo_de_Credito/par1.png)



### Parámetros iniciales

En esta sección, deberemos establecer la unidad de pérdida, que no es mas que la unidad de medida que inteta cuantificar la pérdida de una forma mas compacta. Deberemos ingresar el porcentaje de recuperación que se espera recuperar luego que un crédito esta en mora. Debemos cargar las probabilidades de incumplimiento de la cartera de clientes, podemos cargar una data propia generada por mecanismos internos o podemos cargar la proveniente de la sección de Score de crédito, en ambos casos la data debe tener la siguiente estructura:

![\label{fig:"sd"}](~/Riesgo_de_Credito/par2.png)

### Resultados

En la sección resultados, podremos escoger el nivel de significancia deseado, para mostrar las metrícas usuales de riesgo, tendremos tambien la opción para descargar el roporte proveniente de esta metodología

![\label{fig:"sd"}](~/Riesgo_de_Credito/par3.png)


## Stress Testing

En esta sección de una manera controlada porcentualmente simularemos resultados extremos de pérdida, para luego mostrar la pérdida esperada tras esto.  

![\label{fig:"sd"}](~/Riesgo_de_Credito/stre.png)


## Segunda sección: Credimetrics

En la segunda sección que se observa en la columna izquierda, consta de las siguientes secciones: créditos, matriz de transición, Pérdida por clase, simulación y resultados, stressting.

![\label{fig:"sd"}](~/Riesgo_de_Credito/sec2.png)

## Créditos

En esta sección, deberemos cargar las exposiciones de los créditos y sus respectivas calificaciones:

![\label{fig:"sd"}](~/Riesgo_de_Credito/exp.png)


## Matriz de trancición

En esta sección cargaremos los datos para el cálculo de la matriz de transición crediticia

### Cálculo de la matriz de transición

En esta sección debemos cargar los datos históricos de migraciones crediticias en periodos de tiempo especificos, es decir, con que calificación inicio el crédito y con cual finalizo.

![\label{fig:"sd"}](~/Riesgo_de_Credito/mtr.png)
 Una vez cargados los datos en la parte inferior se mostrara la matriz de transición cálculada:
 
![\label{fig:"sd"}](~/Riesgo_de_Credito/mtr2.png)

### Selecección de la matriz de transición

En esta sección se debera seleccionar la matriz de transición, se puede escoger la ya calculada o en caso de que el banco no posea los datos históricos necesarios, podra cargar su matriz de transición propia, pero debe tener el siguiente formato.


![\label{fig:"sd"}](~/Riesgo_de_Credito/mtr3.png)

## Pérdida por clase.

En esta sección se calcula o solicita las pérdidas esperadas por calificación crediticia.

### Cálculo de pérdida esperada

En esta pestaña el usuario debe cargar los datos históricos de las pérdidad ocacionadas por clientes dado su calificación.

![\label{fig:"sd"}](~/Riesgo_de_Credito/perd1.png)

### Pérdida esperada

Al igual que en la sección de la matriz de transición, el usuario podra seleccionar la matriz calculada o introducir una propia:

![\label{fig:"sd"}](~/Riesgo_de_Credito/perd2.png)

##  Simulación y resultados.

En esta sección el usuario indicara el número de simulaciones para la simulación de MonteCarlo, la aplicación realizara los cálculos y nos presentara las métricas de riesgo y la opción de descargar el reporte.

![\label{fig:"sd"}](~/Riesgo_de_Credito/var2.png)



## Stress Testing.

Una vez llevado a cabo todos los pasos requeridos para la metodología, el usuario podrá realizar una prueba de estres sobre los parámetros del modelo.

![\label{fig:"sd"}](~/Riesgo_de_Credito/stress2.png)

## Tercera sección: Indicadores contables

En esta sección se calculan indicadores contables, poniendo atención al RAROC de crédito, y otros que dependen exclusimamente del balence de la entidad financiera.

## Cálculo del RAROC

Dedicamos una subsección completa debido a su gran uso en las entidades financieras. Lo primero que veremos es una sección informativa:

![\label{fig:"sd"}](~/Riesgo_de_Credito/cont1.png)

### RAROC

En esta pestaña, requerimos que el usuario cargue la información del balence necesaria para el cálculo del RAROC y la metodología de riesgo de crédito que uso



![\label{fig:"sd"}](~/Riesgo_de_Credito/cont2.png)








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

