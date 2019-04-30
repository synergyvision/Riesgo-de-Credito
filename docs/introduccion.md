--- 
title: "Riesgo de Crédito"
subtitle: "Ciencia de los Datos Financieros"
author: "Synergy Vision"
date: "2019-02-15"
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
## Prácticas interactivas con R {-}
## Agradecimientos {-}

<!--chapter:end:index.Rmd-->


# Acerca del Autor {-}

Este material es un esfuerzo de equipo en Synergy Vision, (<http://synergy.vision/nosotros/>).		 

El propósito de este material es ofrecer una experiencia de aprendizaje distinta y enfocada en el estudiante. El propósito es que realmente aprenda y practique con mucha intensidad. La idea es cambiar el modelo de clases magistrales y ofrecer una experiencia más centrada en el estudiante y menos centrado en el profesor. Para los temas más técnicos y avanzados es necesario trabajar de la mano con el estudiante y asistirlo en el proceso de aprendizaje con prácticas guiadas, material en línea e interactivo, videos, evaluación contínua de brechas y entendimiento, entre otros, para procurar el dominio de la materia.
  		  
Nuestro foco es la Ciencia de los Datos Financieros y para ello se desarrollará material sobre: **Probabilidad y Estadística Matemática en R**, **Programación Científica en R**, **Mercados**, **Inversiones y Trading**, **Datos y Modelos Financieros en R**, **Renta Fija**, **Inmunización de Carteras de Renta Fija**, **Teoría de Riesgo en R**, **Finanzas Cuantitativas**, **Ingeniería Financiera**, **Procesos Estocásticos en R**, **Series de Tiempo en R**, **Ciencia de los Datos**, **Ciencia de los Datos Financieros**, **Simulación en R**, **Desarrollo de Aplicaciones Interactivas en R**, **Minería de Datos**, **Aprendizaje Estadístico**, **Estadística Multivariante**, **Riesgo de Crédito**, **Riesgo de Liquidez**, **Riesgo de Mercado**, **Riesgo Operacional**, **Riesgo de Cambio**, **Análisis Técnico**, **Inversión Visual**, **Finanzas**, **Finanzas Corporativas**, **Valoración**, **Teoría de Portafolio**, entre otros.

Nuestra cuenta de Twitter es (https://twitter.com/bysynergyvision) y nuestros repositorios están en GitHub (https://github.com/synergyvision).
  		  
 **Somos Científicos de Datos Financieros**

<!--chapter:end:000-author.Rmd-->

\mainmatter

# Introducción 

En este texto estudiaremos algunas metodologías mas usados para medir el riesgo de créditicio. A lo largo de la historia la necesidad de las intituciones financieras de tener una medida que permita tomar previsiones en momentos de crisis ha ido evolucionando a la mano con el desarrollo de las teorías y modelos estadísticos. Esta necesidad es natural, debido a que cuando se realiza un prestamo, el banco esta corriendo el riesgo de que en cualquier instante el cliente deje de cumplir sus compromisos, este incumplimiento puede ocurrir desde el primer momento en que deba empezar a pagarse el prestamo o al final del mismo, por lo que el banco esta expuesto independientemente de la confianza que tenga en el cliente a perder el 100% del monto dado (a menos que hayan garantias). Un banco puede tener una cartera de miles inclusive millones de clientes, por lo tanto esta expuesto constantemente a sufrir pequeñas pérdidas que ocasionen los incumplimientos, por lo que la institución debe contar con fondos para poder cubrir dichos riesgos, es ridiculo pensar que la institución deba guardar la misma cantidad fondos de lo que presta, por lo tanto debe crear mecanismos estadisticos o contables que permitan medir de manera objetiva los fondos que deben poseerse para el resguardo del banco y permitir superar posibles crisis económicas. En este texto hacemon una descripción de las principales metodologías que se usan para cuantificar el riesgo de crédito, mas precisamente las metologías CreditRisk+ y Credimetrics.


Como hemos mencionado, las herramientas provenientes de la estadistica y matemáticas son fundamentales para la construcción de las metodologías, por lo que en el primer capítulo realizemos un recuento de las principales definiciones de la probabilidad y estadística que serán usadas a lo largo de todo el texto.

Mencionamos que los bancos estan propensos a que los clientes incumplan, por lo cual es natural asignarle a cada cliente una probabilidad de que caiga en mora, aunque a primera instancia pareciera facil dicha asignación, esta puede ser tan complicada como queramos, pues una población puede estar compuesta por varios sectores, los cuales pueden ser tan heterogeneos como queramos. Los bancos contruyen herramientas para valor o score a los clientes y a través del puntaje hallar la probabilidad de que los clientes incumplan sus compromisos. Este tipo de metodologías suelen recibir el nombre de CreditScore o Scoring de crédito. En el capitulo 3 damos una descripción de uno de los CreditScore mas populares usando métodos lineales generalizados.


En el capítulo 5 y  6 daremos una descripción de las metologías CreditRisk+ y Creditmetrics, la primera mucho mas estadística y propensa a crear errores de aproximacón grande si no se cuenta con una buena herramienta de Score, ádemas tiene supuestos fuertes relacionados con la independencia entre los prestamos y pide que las probabilidades de incumplimiento sean bajas, en caso contrario no será ideal su uso, la segunda credimetrics es una herramienta mas precisa y mas facil de implementar pero con la desventaja de que necesita para sus cálculos información que solo esta disponible en mercados desarrollodos, por lo que usarla en mercados emergentes, suele ser complicado por la falta de datos históricos y de registros.

Una vez presentado este texto se espera que personal encargado de la áreas de crédito pueda adaptar la información disponible y poder realizar una buena aproximación de las métricas de riesgo y así lograr que la institución financiera sea capaz de tomar previsiones para los momentos de crisis, que suelen ser imprevistos y algunas veces aleatorios.















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
## La agrupación por bandas de exposición por bandas de exposición a pérdidas
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
## Cartera de dos de créditos con probabilidades de treansición independientes.
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

# Manual técnico de la aplicación Vision Credit Risk

<!--chapter:end:303-ManuelVisionRisk.Rmd-->


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

