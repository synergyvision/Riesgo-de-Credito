--- 
title: "Riesgo de Crédito"
subtitle: "Ciencia de los Datos Financieros"
author: "Synergy Vision"
date: "2018-11-21"
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


## Motivation

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

\mainmatter

# Conceptos de la gestión del riesgo de crédito

A la hora de aplicar o construir cualquier modelo matemático debemos conocer los conceptos que están involucrados en la integración de este, la falta de dicho conocimiento casi seguramente nos conducirá a tesis erradas y modelos mal elaborados. El manejo del riesgo de crédito no se escapa a esto, en el presente capítulo definiremos las nociones y conceptos básicos que estarán presente prácticamente en todos los capítulos del libro.  

## Probabilidad de incumplimiento (DP)

La tarea de asignar una probabilidad de incumplimiento (default probability) a cada uno de los clientes de la cartera de crédito del banco dista mucho de ser fácil. Hay esencialmente dos enfoques para las probabilidades de incumplimiento:  

* Aproximación a partir de datos de mercado:

El más famoso representante de este tipo de aproximación es el concepto de de frecuencias por defecto esperadas (expected default frequencies [EDF]) de KVM Corporation.

Otro método para calibrar las probabilidades de incumplimiento a partir de los datos de mercado se basa en los márgenes de crédito de los productos negociados que conllevan riesgo de crédito.

* Aproximación a partir de ratings:

En esta aproximación, la probabilidad de incumplimiento esta asociada con ratings, y estos ratings son asociado a los clientes por agencias externas de ratings o por metodologías internas de la institución financiera.



<!--chapter:end:200-nociones.Rmd-->

\mainmatter

# Scoring de credito

Los métodos o modelos de credit scoring, a veces denominados score-cards o classifiers, son algoritmos que de manera automática evalúan el riesgo de crédito de un solicitante de financiamiento o de alguien que ya es cliente de la entidad. Tienen una dimensión individual, ya que se enfocan en el riesgo de incumplimiento del individuo o empresa, independientemente de lo que ocurra con el resto de la cartera de préstamos. Este es uno de los aspectos en los que se diferencian de otras herramientas de medición del riesgo de crédito, como son los modelos de cartera y los VaR marginales, que tienen en cuenta la correlación de la calidad crediticia de los deudores de una cartera de préstamos. 

Los modelos de scoring fueron introducidos a partir del año 1970 en el análisis del otorgamiento del crédito, pero generalizados después de 1990 gracias al desarrollo estadístico y tecnológico. Entre los métodos para la construcción de modelos de scoring se pueden tener en cuenta como los más comunes, el modelo LOGIT, PROBIT, las Redes Neuronales y el Análisis Discriminante.

El resultado de la evaluación se refleja en la asignación de alguna medida que permita comparar y ordenar a los evaluados en función de su riesgo, a la vez que cuantificarlo. Por lo general, los modelos de credit scoring le asignan al evaluado un puntaje o score, o una calificación, clasificación o rating. Algunos métodos los asignan a grupos, en donde cada grupo tiene un perfil de riesgo distinto; sin embargo, en la práctica esto equivale a una calificación. A su vez, estos ordenamientos de los deudores permiten obtener estimaciones más concretas del riesgo; en general se busca obtener alguna estimación de la probabilidad de incumplimiento del deudor (PD, por probabilidad de default) asociada a su score, rating o calificación

## Estructura del Scoring de Crédito

Cuando al plantear un modelo la variable dependiente o a explicar toma valores discretos, se emplean modelos de regresión discreta. El caso más simple se da cuando ella es binaria y toma los valores 0 o 1, y se puede estimar con distintos enfoques como el modelo de probabilidad lineal, análisis discriminante, los modelos de tipo probit y logit o con una regresión logística. 

Sea $Y$ una variable aleatoria binaria que toma el valor 1 si ocurre el evento (el deudor cumple con los pagos normalmente) y 0 si entra en mora, se cuenta con una muestra aleatoria de $n$ observaciones, $Y_i$, $i: 1,...,n$, y se define como $\Omega_i$ al conjunto de información relevante asociado con el individuo $i$, que se utilizará para explicar a $Y_i$.

Un modelo de elección binaria es un modelo de la probabilidad de ocurrencia del evento $Y_i$ condicional en el conjunto de información $\Omega_i$. 

$$
\begin{equation}
P_i = Pr(Y_i=1|\Omega_i)
(\#eq:ecuacion1) 
\end{equation}
$$
Dado que $Y_i$ toma los valores 0 y 1, la esperanza de $Y_i$ condicional en $\Omega_i$ es: $$E(Y_i|\Omega_i)=(1*P_i)+(0*(1-P_i))=P_i=Pr(Y_i=1|\Omega_i)
(\#eq:ecuacion2)$$

En consecuencia, cuando la variable a explicar es binaria, su esperanza condicional es la probabilidad condicional de ocurrencia del evento. 

##El modelo de probabilidad lineal

Supóngase que $\Omega_i$ está constituido por un vector columna $X_i$ compuesto por $k$ variables explicativas, incluyendo a la ordenada al origen, $\beta$ es un vector columna que contiene los parámetros correspondientes a las variables explicativas, y que se intenta modelar a la variable $Y_i$ a través del modelo de probabilidad lineal, postulando la siguiente relación: 

$$Y_i=\beta^TX_i+\epsilon_i,\quad\textrm{donde}\quad E(\epsilon_i|X_i)=0\quad \textrm{y}\quad E(\epsilon_i)=0 
(\#eq:ecuacion3)$$

y usando \@ref(eq:ecuacion2)

$$E(Y_i|X_i)=P_i=\beta^TX_i
(\#eq:ecuacion4)$$

El modelo de probabilidad lineal, como se observa en la ecuación \@ref(eq:ecuacion3), implica estimar un modelo lineal en los parámetros para $Y_i$. Los valores predichos deberían en su mayoría ubicarse en el intervalo $[0,1]$, pudiendo ser interpretados como la probabilidad de que la variable a explicar tome alguno de estos valores. 

Mientras que su estimación e interpretación es simple, su utilización se ha visto desalentada por dos problemas en la metodología. En primer lugar, como la esperanza condicionada de $Y_i$ es igual a la probabilidad condicionada de ocurrencia del evento (de $Y_i=1$), ella debería estar restringida al intervalo $[0,1]$. Sin embargo, el modelo lineal no impone ninguna restricción sobre $\beta^TX_i$i, asumiendo implícitamente que la variable dependiente puede tomar cualquier valor. Es por esto que el modelo puede estimar probabilidades negativas o mayores que uno, lo cual carece de significado económico. A su vez, el término de error de este modelo no es homocedástico, ya que la varianza condicional varía según las observaciones, por lo que las estimaciones de $\beta$ no son eficientes. 

Para resolver estos inconvenientes hay modelos econométricos, generalmente estimados por máxima verosimilitud, que tienen en cuenta la naturaleza discreta de la variable dependiente: se trata de los modelos de respuesta o elección binaria. Ellos utilizan ciertas funciones de distribución para la innovación, con el objetivo de limitar las probabilidades estimadas al intervalo [0,1]: las más usadas son la función de probabilidad acumulada normal estándar y la función logística. Cuando se usa la normal estándar se trata de un modelo probit, y logit cuando se usa la función logística. 

### Los modelos probit y logit.

El modelo probit es una de varias alternativas para estimar modelos de respuesta binaria. La idea consiste en utilizar una función de transformación F(x) que tenga las siguientes propiedades: 

$$F(-\infty)=0, \quad F(\infty)=1\quad\textrm{y}\quad f(x)\equiv\frac{dF(x)}{dx}>0
(\#eq:ecuacion5)$$

$F(x)$ es una función monótona creciente que mapea de la línea real al intervalo [0,1]. Varias funciones de distribución acumulada tienen estas propiedades: la normal, la logística, la de Cauchy y la de Burr, entre otras. Estas distintas alternativas para los modelos de respuesta binaria consisten en una función de transformación F(x) aplicada a una función índice que depende de las variables explicativas del modelo y que tiene las propiedades de una función de regresión, pudiendo ser lineal o no lineal. 

La siguiente es una especificación general para cualquiera de los modelos de elección binaria: 

$$E(Y_i|\Omega_i)=F(h(X_i))
(\#eq:ecuacion6)$$

Donde $h$ es la función índice.

Si bien $h$ puede ser cualquier función, en general se usa una función lineal, es decir:

$$E(Y_i|\Omega_i)=F(\beta^TX_i)
(\#eq:ecuacion7)$$

Por lo cual el modelo de elección binaria es simplemente una transformación no lineal de una regresión lineal, y si bien $\beta^TX_i$ puede tomar cualquier valor sobre la línea real, $F(\beta^TX_i)$ está limitado al intervalo [0,1]. En el modelo probit, la función de transformación $F(x)$ es la función de distribución acumulada normal estándar, y por definición satisface las condiciones impuestas en \@ref(eq:ecuacion5). En este caso, el modelo de elección binaria puede escribirse de la siguiente manera:

$$P_i=E(Y_i|\Omega_i)=F(\beta^TX_i)=\Phi(\beta^TX_i)=\int_{-\infty}^{\frac{\beta^TX_i}{\sigma}}\frac{e^{\frac{-s^2}{2}}}{\sqrt{2\pi}}ds
(\#eq:ecuacion8)$$

Cuando se trata del modelo logit, $F(x)$ es la función logística y el modelo de respuesta binaria se escribe como:

$$P_i=E(Y_i|\Omega_i)=F(\beta^TX_i)=\Phi(\beta^TX_i)=\frac{e^{\beta^TX_i}}{1+e^{\beta^TX_i}}
(\#eq:ecuacion9)$$
 
Los modelos probit y logit pueden ser derivados de otro modelo que introduce una variable no observada o latente $y^*$, de la siguiente manera. Sea:

$$y^*_i=\beta^TX_i+\epsilon_i, \quad \textrm{con} \quad \epsilon_i\sim iid(0,1)
(\#eq:ecuacion10)$$

Si bien $y^*$ no se observa, decimos que:

$$Y_i=1\quad\quad\textrm{si} \quad y^*>0 \quad \textrm{y} \quad Y_i=0\quad\quad\textrm{si} \quad y^*<0
(\#eq:ecuacion11)$$

Luego, la probabilidad que $Y_i=1$ viene dada por: 




\begin{equation} \label{eq1}
\begin{split}
P(Y_i=1) & = P(y_i^*>0)=P(\beta^TX_i+\epsilon_i>0)= 1-P(\beta^TX_i+\epsilon_i\leq 0)=1-P(\epsilon_i\leq -\beta^TX_i)\\
 & = 1-F(-\beta^TX_i)=F(\beta^TX_i)
\end{split}
\end{equation}

ya que se supone que εi tiene una distribución simétrica. Cuando $\epsilon_i ~ N(0,1)$ $F$ es $\Phi$, la función de distribución de probabilidades acumuladas normal estándar y se trata del modelo probit, mientras que si F es la función logística se trata de un modelo logit y su densidad también es simétrica alrededor de cero.

###Estimación de los modelos logit y probit

La estimación de estos modelos se hace por el método de máxima verosimilitud. Con métodos numéricos se buscan los valores de $\beta$ que maximizan la siguiente función logarítmica de verosimilitud:

$$l(\beta)=\sum^{n}_{i=1}\big{(}Y_ilog\big{(}F\big{(}\beta^TX_i\big{)}\big{)}+\big{(}1-Y_i\big{)}log\big{(}1-F\big{(}\beta^TX_i\big{)}\big{)}\big{)}
(\#eq:ecuacion12)$$

Las condiciones de primer orden para un máximo en \@ref(eq:ecuacion12) son:

$$\sum_{i=1}^n\frac{(Y_i-\hat{F_i)}\hat{f_i}X_{ij}}{\hat{F_i}(1-\hat{F_i})}=0,\quad j=1,...,k
(\#eq:ecuacion13)$$

donde $\hat{F_i}\equiv F_i(b^TX_i) \quad \textrm{y} \quad \hat{f_i}\equiv f_i(b^TX_i)$

Los modelos probit, logit y otros tipos de modelos de respuesta binaria satisfacen las condiciones de regularidad necesarias para que las estimaciones de los parámetros sean consistentes y asintóticamente normales, con la matriz de covarianzas asintótica dada por la inversa de la matriz de información.

### Interpretación del modelo

En el contexto de los modelos de credit scoring se puede asociar $\beta^TX_i$ a la calidad crediticia del individuo (variable latente o no observada). Cambiando su denominación por $Z_i$, esta variable representa la calidad crediticia del individuo, que se puede suponer el resultado de una función lineal en sus parámetros, como por ejemplo:

$$Z_i=\beta_0+\beta_1X_{1j}+\beta_2X_{2j}+...+\epsilon_i
(\#eq:ecuacion14)$$

Las estimaciones de los parámetros $\beta_j$ se obtienen por máxima verosimilitud como se explicó en los párrafos anteriores, y las variables $X_j$ contienen la información de los deudores. Habiendo obtenido las estimaciones $b_j$, el modelo empírico con el que trabajará el analista de riesgo es,

$$Z_i=\beta_0+\beta_1X_{1j}+\beta_2X_{2j}+\beta_3X_{3j}
(\#eq:ecuacion15)$$

cuando se trata de un modelo que emplea cinco variables ($j=5$). La variable $z_i$ es el score estimado del deudor, una medida de su calidad crediticia obtenida a partir de los parámetros estimados y de su propia información. Este score, aplicado a las funciones de distribución de probabilidades acumuladas normal o logística, permite conocer la probabilidad de incumplimiento y en consecuencia el riesgo del deudor.

### Relación entre el score y el riesgo

Habiendo definido al score, como $$P(Y_i=1)=F(Z_i)$$ y $$P(Y_i=0)=1-F(Z_i)$$

donde queda claro que cambios en $Z_i$ implican cambios en la probabilidad de incumplimiento (PD) del individuo. La relación entre score y riesgo (la PD) no es lineal, por lo que el cambio en el riesgo derivado de un cambio en el score depende de los valores que este último tome. Para valores del score muy bajos, un aumento en el mismo produce una rápida subida en la probabilidad de cumplimiento y una rápida disminución de la PD, mientras que para valores del score altos, una mejora en el mismo hace que la probabilidad de incumplimiento aumente poco y genera una leve caída en el riesgo. Es decir, cuanto mayor es el score, menor es la caída en el riesgo derivada de un aumento en el primero.

### Uso de modelos lineales para la selección de variables (Métodos de búsqueda secuencial).

Los métodos de búsqueda secuencial tienen en común la aproximación general de estimación de las ecuaciones de regresión con un conjunto de variables y a continuación añadir o eliminar selectivamente variables hasta que se consiga alguna medida criterio conjunta. Esta aproximación proporciona un método objetivo de selección de variables que maximizan la predicción con el número más pequeño de variables empleadas. Existen dos tipos de aproximaciones de búsqueda secuencial: (1) la estimación por etapas, y (2) la eliminación progresiva y regresiva. En cada aproximación, se valoran las variables individualmente en función de su contribución a la predicción de la variable dependiente y se añaden o eliminan según su contribución relativa.


#### Estimación por etapas (paso a paso o stepwise)

La estimación por etapas es quizá la aproximación más popular para seleccionar variables. Esta aproximación permite examinar la contribución de cada variable predictor al modelo de regresión. Se considera la inclusión de cada variable antes de desarrollar la ecuación. Se añade primero la variable independiente con la contribución más grande. Las variables independientes se seleccionan entonces para la inclusión basada en su contribución incremental sobre la(s) variable(s) ya existente(s) en la ecuación. Las cuestiones específicas en cada etapa son:

1. Empezar con el modelo de regresión simple en el cual sólo se utiliza la única variable predictor que es la que se muestra más altamente correlacionada con la variable criterio. La ecuación sería $Y= \beta_0 Q + \beta_1X_1$.

2. Examinar los coeficientes de correlación parcial para encontrar una variable predictor adicional que explique además de una parte significativa, la mayor parte del error que queda de la primera ecuación de regresión.

3. Recalcular la ecuación de regresión utilizando las dos variables predictor, y examinar el valor parcial $F$ de la variable original del modelo para ver si todavía realiza una contribución significativa, dada la presencia de la nueva variable predictor. Si no lo hace, eliminamos la variable. Esta capacidad de eliminar variables presentes en el modelo distingue el modelo por etapas de los modelos de adición progresiva/eliminación regresiva. Si las variables originales todavía representan una contribución significativa, la ecuación sería $Y = \beta_0 + \beta_1X_1 + \beta_2X_2$.

4. Continúa este procedimiento examinando todos las variables independientes no presentes en el modelo para determinar si deberían incluirse en la ecuación. Si se incluye una nueva variable independiente, hay que examinar todos los predictores previamente incluidos en el modelo para juzgar si se deben mantener. Existe un sesgo potencial en el procedimiento por etapas que resulta de considerar sólo una variable a seleccionar cada vez. Supongamos que las variables $X_3$ y $X_4$ explicaran conjuntamente una parte significativa de la varianza (cada una considerando la presencia de la otra), pero no son significativas por sí solas. En esta situación, ninguna debería ser considerada para el modelo final.

#### La adición progresiva (forward) y la eliminación regresiva (backward)

La adición progresiva y la eliminación regresiva son fundamentalmente procesos de ensayo y error para buscar los mejores estimadores de la regresión. El modelo de adición progresiva es similar al procedimiento por etapas arriba explicada, mientras que el procedimiento de eliminación regresiva implica calcular una ecuación de regresión con todas las variables independientes, para a continuación ir eliminando las variables independientes que no contribuyan significativamente. La distinción principal de la aproximación por etapas respecto de los procedimientos de adición progresiva y eliminación regresiva es su capacidad de añadir o eliminar las variables en cada etapa. Una vez que se añade o elimina una variable en las esquemas de adición progresiva o eliminación regresiva, no existe posibilidad de revertir la acción posteriormente.

#### Advertencias sobre los métodos de búsqueda secuenciales

El investigador debe ser consciente de dos advertencias cuando se usa cualquier procedimiento de búsqueda secueneial. En prim er lugar, la multicolinealidad entre variables independientes puede tener un impacto sustancial sobre la especificación final del modelo. Examinemos esta situación con dos variables altamente correlacionadas que tienen similares correlaciones con la variable independiente. El criterio de inclusión o eliminación en estas aproximaciones es maximizar el incremento de potencia predictiva de la variable adicional. Si una de estas variables entra en el modelo de regresión, es muy probable que la otra variable también entre, dado que estas variables están altamente correlacionadas y existe poca varianza singular para cada variable por separado. 

Por esta razón, se deben evaluar los efectos de la multicolinealidad en la interpretación del modelo y examinar las correlaciones directas de todas las variables independientes potenciales. Esto ayudará a evitar concluir que las variables independientes que no entren en el modelo no sean trascendentes cuando en realidad están altamente relacionadas con la variable dependiente, pero también correlacionadas con las variables ya existentes en el modelo. Aunque las aproximaciones de búsqueda secueneial maximizarán la capacidad predictiva del modelo de regresión, el investigador debe ser cuidadoso en la interpretación del modelo. Una segunda advertencia pertenece principalmente al procedimiento por etapas. En esta aproximación, los test de significación múltiple se realizan en el proceso de estimación del modelo. Para asegurar que la tasa de error conjunto a lo largo de todos los test de significación es razonable, el investigador debería emplear umbrales muy conservadores (por ejemplo, 0,01) al añadir o sacar las variables.






<!--chapter:end:300-Scoring.Rmd-->


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

