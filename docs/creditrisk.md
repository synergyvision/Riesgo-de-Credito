--- 
title: "Riesgo de Crédito"
subtitle: "Ciencia de los Datos Financieros"
author: "Synergy Vision"
date: "2019-04-05"
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

\mainmatter

# CreditRisk+

**CreditRisk+** es un modelo de impago en el que se parte de la idea de que los eventos de incumplimiento de los diferentes deudores son eventos *Bernoulli independientes*. Sin embargo, es importante tener en cuenta que los eventos de incumplimiento de deudores ocurren de manera causística en el tiempo y es imposible pronosticar el momento preciso en el que ocurrirán dichos eventos y el numero de eventos que sucederan en un cierto intervalo de tiempo.

El desarrollo de una teoría estadística que explique estos procesos,dentro del contexto de riesgo crediticio, comienza considerando el caso simple de una caertera de créditos que incluye a $N$ deudores, a cada uno de los cuales se les puede asociar una *probabilidad fija de incumplimiento*; es decir, se conoce: $$p_i=\textrm{Probabilidad de incumplimiento del deudor } i$$

La distribuciíon de pérdidad que puede resultar del incumplimiento de los deudores de todo el portafolio la obtiene **CreditRisk** de manera indirecta, a través de las funciones de probabilidad, y procede en dos pasos. Primero, obtenemos la función generadora de probabilidad (FGP) del numero de incumplimientos y después, haciendo un supuesto sobre el nivel de pérdidas asociadas al incumplimiento de cada deudor, obtiene la FGP las pérdidas que puede ceder la cartera. A continuación se obtiene la primera para el caso simple bajo consideración.

## La distribución del número de incumplimientos con probabilidades de impago fijas.


La FGP de números de incumplimientos se define como: $$F(s)=\sum_{n=0}^{\infty}Prob(n \textrm{ incumplimientos})\textrm{x}s^n$$

Si la cartera tuviera un sólo deudor, lo único que puede suceder es que éste cumpla o incumpla. Por lo tanto, la FGP de incumplimientos para un sólo deudor es simplemente:$$F_i(s)=(1-p_i)s^0+p_i(s-1)=1+p_i(s-1)$$ 

Como se considera que los eventos de incumplimiento son independientes, la FGP de toda la cartera es el producto de las FGP individuales, es decir:$$F(s)=\prod_{i=1}^NF_i(s)=\prod_{i=1}^N\big{[}1+p_i(s-1)\big{]}$$
Aplicando logaritmo a ambos lados de la expresión, se obtiene:$$Ln(F(s))=\sum_{i=1}^NLn\big{[}1+p_i(s-1)\big{]}$$
Generalmente, las probabilidades de incumplimiento de obligaciones crediticias a nivel individual son pequeñas y, por lo tanto, las potencias de estas son más pequeñas y pueden ser ignoradas.

Esto implica que para los valores pequeños de $p_i$:$$Ln\big{[}1+p_i(s-1)\big{]}\approx p_i(s-1)$$
Lo anterior es el gran supuesto de **CreditRisk+**, que inituitivamente dice que mientras la probabilidad de incumoplimiento se $p_i$ sea "pequeña", se puede ignorar el hecho de que un deudor no puede incumplir más que una sola vez. Haciendo esta sustitución, se obtiene:$$Ln(F(s))=\sum_{i=1}^Np_i(s-1)$$
Lo anterior conduce directamente a: $$F(s)=e^{\sum_{i=1}^Np_i(s-1)}=e^{\mu(s-1)};\textrm{ donde } \mu=\sum_{i=1}^Np_i$$


La FGP corresponde a la FGP de la distribución de *Poisson* con parámetro $\mu$, a continuación se comprueba esto, usando el desarrollo de Taylor, es decir:$$F(s)=\sum_{n=}^\infty\mu^ne^{-\mu}s^n$$
Por lo tanto, bajo el supuesto que las probabilidades de incumplimientos son bajas, obtenemos que:$$Prob(\textrm{ número incumplimientos }=n)=\frac{1}{n!}\mu^ne^{-\mu}$$

En la expresión anterior,'$\mu$' es la *tasa promedio de incumplimiento* y es el único parámetro de la distribución. Como "$N$", la distribución es independiente del número de deudores y de las probabilidades individuales de impago, siempre y cuando éstas sean "uniformemente pequeñas". Sin embargo queda claro que los deudores tengan la misma probabilidad de incumplir. Es más, éstas pueden ser diferentes y el cálculo de la tasa esperado de incumplimiento, sólo depende de la existencia de esta informacíon.

Para concluir con este tema, es impotante hacer hicapié en que el supuesto clave que permite llegar a la distribución Poisson es cuando un número no-negativo "$y$" es muy pequeño ($y\approx 0$), se puede utilizar la aproximación "$Ln(1+y)\approx y$". Esta forma exponencial de la FGP de la distribución de Poisson, es la clave de la facilidad de cálculo que tiene **CreditRisk+**.

Además, es necesario señalar que en todo el análisis anterior hay implícito un período de tiempon que está relacionado con el parámetro de la distribución. Así, al se\tribución obtenida debe interpretarse de la misma manera, es decir:$$\frac{1}{n!}\mu^ne^{-\mu}=\textrm{ Probabilidad de que ocurran } n \textrm{ incumplimientos }$$

## La agrupación por bandas de exposición por bandas de exposición a pérdidas

Dado que pueden existir varios deudores que implican niveles de pérdidas semejantes, **CreditRisk+** empieza por agrupar a los deudores por bandas de exposiciónm oiguales. Aunque esta agrupación introduce errores de rodondeo en la estimación de la distribución, facilita mucho el desarrollo del modelo y reduce significativamente el número de datos requeridos para realizar los cálculos. Además, si el número de niveles de exposición es grande y el ancho de las bandas es pequeño, en relación con el tamaño de exposición promedio de la cartera, se puede demostrar que el error introducido es despreciable. De hecho, en la práctica es muy difícil determinar *a priori* el nivel de exposición y, por lo tanto, éste no debe representar un elemento crítico en la estimación del riesgo total de la cartera.

Así, supóngase que la perdida esperada por deudor que cae en impago es una proporción fija "$\lambda_i$" del monto total "$D_i$" que debe el deudor "$i$". A su vez, para mantener la elegancia del modelo, supóngase que el nivel de exposición que representa el deudor para el acreedor "$L_i$" en un múltiplo entero de una únidad fija de pérdida "$L$". Estos múltiplos enteros de $L$ reciben el nombre de "*niveles de exposición estandar*". Aasí la pérdida que puede representar el incumplimiento del deudor "$i$" para el acreedor, se mide en términos de múltiplos de "$v_i$", de la únidad fija de pérdida $L$, y es simplemente la cantidad siguiente:$$v_i=\textrm{ Redondeo }\bigg{(}\frac{\lambda D_i}{L}\bigg{)}$$
De esto se obtiene el  nivel estándar de exposición que representa cada deudor para el acreedor, mediante la identidad siguiente:$$L_i=L_{v_i}$$

## La determinación de la *distribución de pérdidas* de la cartera.

El último paso es encontrar la *la distribución de pérdidas de la cartera*, para lo cual se procede de la misma manera, es decir, encontar la FGP de dicha distribución. Nótese que la distribución de pérdidas es necesariamente diferente a la delmnumero de impagos, ya que puede resultar un cierto nivel de pérdidas con diferentes combinaciones de incumplimientos de deudores. Por ejemplo una pérdida de $100.000$ dolares puede ser la consecuencia del incumplimiento que debe esta cantidad o del incumplimiento de $10$ de deudores que deben $10.000$ dolares cada uno. Además, el conocimiento de la forma en que están distribuidos los diferentes niveles de exposición entre los  diferentes deudores que componen la cartera es indispensable para poder obtener la distribución de pérdidas. Contrasrio a lo que sucede con las diferencias entre probabilidades de incumplimiento individuales, que son pequeñas aun en términos relativos y por lo tanto no afectan la distribución del número de incumplimiento, las diferencias en los niveles de exposición a los deudores representados en la cartera si son significativos y, por lo tanto, la distribución de pérdidas resultante no tiene por que ser *Poisson*. Sin embargo, aunque no se va a poder obtener una formula explícita de la distribución de pérdidas, se verá que es poisible obtener una expresión cerrada sencilla de la FGP, que se presta fácilmente al cálculo de las probabilidades correspindientes.

Así recordemos que la FGP de las pérdidas es: $$G(s)=\sum_nProb(\textrm{ Pérdida acumulada}=n\textrm{x}L)S^n$$

Sea "$G_j$" la FGP de las pérdidas de la banda "$j$", la probabilidad de que se pierda $n\times v_j$ unidades en la banda "$j$" es igual a la probabilidad de que "$n$" deudores de esta banda incumplan, y ya se sabe que el incumplimiento de estos deudores sigue una distreibución de *Poisson*, es decir:$$\textrm{ Prob. de }n\textrm{ incumplimientos en la banda }j=\frac{1}{n!}\mu_j^ne^{-\mu_j}$$

Por lo tanto:$$G_j(s)=\sum_{n=0}^\infty \frac{1}{n!}\mu_j^ne^{-\mu_j}s^{nv_j}=e^{-\mu_j}\sum_{n=0}^\infty \frac{1}{n!}(\mu_js^{v_j})^n=e^{-\mu_j}e^{\mu_js^{v_j}}=e^{\mu_j(s^{v_j}-1)}$$

Suponiendo independencia de eventos de incumplimientos, las pérdidas asociadas a cada banda tambien serán independientes. En este caso, la FGP de las pérdidas de la cartera de crédito es la suma de las pérdidas ocurridas en cada banda, que por propiedades de las funciones generadoras de probabilidad es el producto de las FGP de las bandas:$$G(s)=\prod_jG_j(s)=\prod_{j=1}^me^{\mu_j(s^{v_j}-1)}=e^{\sum_{j=1}^m\mu_j(s^{v_j}-1)}$$es decir:$$G(s)=e^{\sum_{j=1}^m\mu_j(s^{v_j}-1)}$$

## Obteniendo la distribución de las pérdidas

Aunque a diferencia de la distribución del número de incumoplimientos, en el caso de las pérdidas no se sabe la forma de la distribución, la probabilidad de que se pierdan $n$ unidades de $L$ en la cartera total de créditos se puede obtener mediante la expansión de Taylor de la expresión anterior; es decir: la probabilidad de quen se pierdan $n$ unidades $L$ es el coeficiente de $s^n$ en la expansión de Taylor. El desarrollo se facilita definiendo el polinomio siguiente:$$P(s)=\frac{1}{\mu}\sum_{j=1}^m\mu_js^{v_j}=\frac{\sum_{j=1}^m\frac{\epsilon_j}{{v_j}}s^{v_j}}{\sum_{j=1}^m\frac{\epsilon_j}{{v_j}}}$$ donde $\epsilon_j$ es la pardida esperada de la banda $j$ y $v_j$ es la exposición común que comparten los deudores de la banda $j$.

Con esto, la FGP de pérdidas se puede expresar de la forma siguiente:$$G(s)=e^{\sum_{j=1}^m\mu_j(s^{v_j}-1)}=e^{\mu[P(s)-1]}=F[P(s)]$$
La función generadora de probabilidades de las pérdidas comprende dos fuentes de incertidumbre, a saber: el comportamiento *Poisson* del numero de incumplimiemntos y la aleatoriedad de las pérdidas dado incumplimiento, que esta asociado a los distintos niveles de exposición del acreedora los diferentes deudores. Notese que la única información requerida para obtener la distribución de pérdidases $\epsilon_j$ y $v_j$, estas parejas representan un número de datos significativamente menor, que si tuviera que manejar información semejante por cada deudor, aun para carteras con un número muy grandem de créditos.
para encontrar la distribución de pérdidas, se parte de la expresión:$$G(s)=\sum_n\textrm{ Prob(pérdidas acumuladas}=n\times L)S^n $$.

Tomando la expansión de Taylor para $G(s)$, la probabilidad de perdida de $nL$ unidades es$$P_n(L)=\frac{1}{n!}\frac{d^nG(s)}{ds^n}\bigg{|}_{s=0}$$. En el apéndice A 4.1 del manual de CreditRisk+ se deduce una fórmula recursiva sencilla para obtener estas probabilidades, las cuales es:$$P_n(L)=\sum_{j|v_j\le n}\frac{\epsilon_j}{n}P_{n-v_j}(L)=\frac{1}{n}\sum_{j|v_j\le n}\epsilon_jP_{n-v_j}(L)$$
donde $P_0(L)=e^{-\mu}$ donde $\mu=\sum_{j=1}^m\mu_j$

## Obtención del VaR

Para obtener el VaR de una cartera de créditos bajo el esquema de CreditRisk+, simplemente se obtiene la distribución de distribución de probabilidad acumulada de las pérdidas y, una vez decidido el nivel de confianza $\alpha$ con el que se quiere trabajar se lee de la tabla el número de unidades estándar de pérdidas que corresponden a una probabilidad acumulada de de pérdida $1-\alpha$. El paso final es convertir las unidades estándar de pérdida a unidades manetarias, multiplicando las unidades de pérdidas correspondientes al nivel de confianza escogido, por la unidas de pérdida $L$.
























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


# Manual técnico de la aplicación Vision CreditRisk

Placeholder


## Presentación de la aplicación.
## Primera sección: CreditRisk+
### Datos
### Estadísticos:
#### Relación de las variables independientes
### Seleccion de variables
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

