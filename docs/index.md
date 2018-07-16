<script src="https://cdn.datacamp.com/datacamp-light-latest.min.js"></script>

--- 
title: "Riesgo de Crédito"
subtitle: "Ciencia de los Datos Financieros"
author: "Synergy Vision"
date: "2018-07-16"
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

<a href="https://synergy.vision/LibrosInteractivos/" target="_blank"><img src="images/cover.png" style="display: block; margin: auto;" /></a>


![Creative Commons License](images/by-nc-sa.png)  
La versión en línea de este libro se comparte bajo la licencia [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-nc-sa/4.0/).

## ¿Por qué  leer este libro? {-}

Este libro es el resultado de enfocarnos en proveer la mayor cantidad de material sobre Probabilidad y Estadística Matemática con un desarrollo teórico lo más explícito posible, con el valor agregado de incorporar ejemplos de las finanzas y la programación en `R`. Finalmente tenemos un libro interactivo que ofrece una experiencia de aprendizaje distinta e innovadora.

El un mundo abierto, ya no es tanto el acceso a la información, sino el acceso al conocimiento. Este libro es la base teórica para nuestro Diplomado en Probabilidades y Estadística Matemática aplicado a las Finanzas. Aunque es un material de corte general, hay ejemplos específicos traido de las finanzas. En el Diplomado nos enfocamos en el participante, el propósito es que el instructor ocupa a lo sumo el 20% del tiempo y el resto del tiempo los participantes se dedican a practicar y resolver ejercicios, tanto teóricos como de programación y modelaje en `R` al nivel de un curso de Postgrado. Ésta es la base de un programa en Ciencia de los Datos Financieros.

Es mucha la literatura, pero son pocas las opciones donde se pueda navegar el libro de forma amigable y además contar con ejemplos en `R` y ejercicios interactivos, además del contenido multimedia. Esperamos que ésta sea un contribución sobre nuevas prácticas para publicar el contenido y darle vida, crear una experiencia distinta, una experiencia interactiva y visual. El reto es darle vida al contenido asistidos con las herramientas de Internet.

Finalmente este es un intento de ofrecer otra visión sobre la enseñanza y la generación de material más accesible. Estamos en un mundo multidisciplinado, es por ello que ahora hay que generar contenido que conjugue en un mismo lugar las matemáticas, estadística, finanzas y la computación.

Lo dejamos público ya que las herramientas que usamos para ensamblarlo son abiertas y públicas.

## Estructura del libro {-}

TODO: Describir la estructura

## Información sobre los programas y convenciones {-}

Este libro es posible gracias a una gran cantidad de desarrolladores que contribuyen en la construcción de herramientas para generar documentos enriquecidos e interactivos. En particular al autor de los paquetes Yihui Xie xie2015.

## Prácticas interactivas con R {-}

Vamos a utilizar el paquete [Datacamp Tutorial](https://github.com/datacamp/tutorial) que utiliza la librería en JavaScript [Datacamp Light](https://github.com/datacamp/datacamp-light) para crear ejercicios y prácticas con `R`. De esta forma el libro es completamente interactivo y con prácticas incluidas. De esta forma estamos creando una experiencia única de aprendizaje en línea.

<div data-datacamp-exercise data-height="300" data-encoded="true">eyJsYW5ndWFnZSI6InIiLCJwcmVfZXhlcmNpc2VfY29kZSI6ImIgPC0gNSIsInNhbXBsZSI6IiMgQ3JlYSB1bmEgdmFyaWFibGUgYSwgaWd1YWwgYSA1XG5cblxuIyBNdWVzdHJhIGVsIHZhbG9yIGRlIGEiLCJzb2x1dGlvbiI6IiMgQ3JlYSB1bmEgdmFyaWFibGUgYSwgaWd1YWwgYSA1XG5hIDwtIDVcblxuIyBNdWVzdHJhIGVsIHZhbG9yIGRlIGFcbmEiLCJzY3QiOiJ0ZXN0X29iamVjdChcImFcIilcbnRlc3Rfb3V0cHV0X2NvbnRhaW5zKFwiYVwiLCBpbmNvcnJlY3RfbXNnID0gXCJBc2VnJnVhY3V0ZTtyYXRlIGRlIG1vc3RyYXIgZWwgdmFsb3IgZGUgYGFgLlwiKVxuc3VjY2Vzc19tc2coXCJFeGNlbGVudGUhXCIpIn0=</div>







## Agradecimientos {-}

A todo el equipo de Synergy Vision que no deja de soñar. Hay que hacer lo que pocos hacen, insistir, insistir hasta alcanzar. Lo más importante es concretar las ideas. La idea es sólo el inicio y solo vale cuando se concreta.


\BeginKnitrBlock{flushright}<p class="flushright">Synergy Vision, Caracas, Venezuela</p>\EndKnitrBlock{flushright}








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

Conocer en qué se invierte es un problema fundamental en la gestión del capital propio o de terceros, es el problema primario en la gestión de inversiones. La gestión de inversiones es un campo con muchos avances que, hoy en día, demanda métodos basados en las ciencias. En particular las matemáticas, probabilidades y la computación.

Un primer avance en la solución de este problema es ofrecido por Harry Markowitz en 1952 e inaugura una nueva era en la aplicación de los métodos matemáticos para obtener carteras de inversión con características especiales, para conocer la relación entre el riesgo y el rendimiento. Este avance es reconocido con el precio Nobel en Economía en 1990 e inaugura la era de las Finanzas Cuantitativas, las Finanzas Computaciones, las Matemáticas Financieras, la Ingenería Financiera, entre otras prácticas.

Markowitz desarrolla  un modelo que considera los retornos de cada instrumento como una variable aleatoria y combina estas variables aleatorias para obtener el riesgo y el rendimiento de dicha combinación. Esta combinación de intrumentos es lo que denominamos portafolios y el objeto de Markowitz es encontrar los portafolios óptimos. Los portafolios óptimos son combinaciones de activos financieros que generan el mayor rendimiento posible con el menor riesgo. La combinación de activos nos lleva a la idea de la diversificación y su efecto en la gestión del riesgo, ya que ahora también es importante la relación que existe entre los instrumentos como un factor determinante del riesgo de la cartera.

Si los instrumentos tienen una correlación positiva, el riesgo no disminuye, sin embargo si la correlación es negativa es posible, mediante la diversificación, disminuir el riesgo de la cartera.

En la práctica es muy complejo obtener instrumentos no correlacionados, ya que los mercados tienen a seguir ciclos alcistas o bajistas y los instrumentos tienen a moverse de forma acompasada. Es decir, que la diversificación no necesariamente logra el objetivo de disminuir el riesgo de la cartera.

Con estos avances ahora el rendimiento no se discute de forma aislada, el rendimiento siempre viene acompañado del riesgo y ahora la selección del inversionista debe considerar ambas variables para tomar su decisión. Este terreno es un campo con muchos avances ya que se aplican diversos métodos de optimización, de simulación o de medición del riesgo para conseguir las carteras que cumplen con la mejor relación para el inversionista.

Este trabajo pretende abordar el campo de la distribución del capital en activos financieros que cumplen con ciertas condiciones partiendo de los datos disponibles. En este trabajo partiremos de la historia de los instrumentos para obtener sus características y conformar portafolios óptimos de acuerdo a la metodología de Markowitz. Adicionalmente se desarrollarán herramientas para facilitar el procesamiento de los datos y obtener dichos portafolios.

## Motivation

La historia reciente de los mercados, se caracteriza por la presencia de varias burbujas o grandes choques en los mercados. En el mercado americano, iniciemos desde 1999-2000 con la burbuja de las empresas tecnológicas que surgen con el auge de Internet y tienen como objetivo el desarrollo de la nueva economía virtual, hoy día es evidente que Amazon y Google son las grandes empresas que capitalizaron gran parte del potencial, el gran impacto generado por la caida de las Torres Gemelas en Septiembre del 2001 y luego la gran crisis inmobiliaria durante el 2007-2008 de la cual se ha recuperado apenas en 2015-2016. Hoy de nuevo, en el año 2017, estamos en presencia de un mercado alcista que alcanza nuevo territorio, ha alcanzado nuevos altos históricos y es inevitable la pregunta: ¿Estamos en presencia de una nueva burbuja?. Esta pregunta no tiene el objetivo de predecir que el mercado va a caer en algún momento, de hecho lo normal es que lo haga, el mercado cumple ciclos contínuamente y no tiene nada de innovador señalar que va a caer cuando hace altos históricos. El mercado se mueve en ondas. Lo que nos interesa de esta dinámica es cómo construir carteras de inversión que puedan sobrevivir estos choques, estos mercados bajistas y al mismo tiempo aprovechar al máximo los mercados alcistas.

Desde 1952 y más notablemente desde 1990 que recibe el premio Nobel de Economía, Harry Markowitz es señalado como el padre de la Teoría Moderna de Portafolio. Su objetivo es definir una metodología que partiendo de un grupo de activos, se generan las carteras eficientes. Mucho ha ocurrido desde que la Economía reconoce como un área de desarrollo la aplicación de los modelos Matemáticos para ofrecer luz sobre la dedicación óptima del capital.

Este reconocimiento al aporte de la ciencia en la Economía genera dos grandes líneas de trabajo que intenta responder dos preguntas fundamentales: ¿Dónde coloco el capital y en qué cantidad? y ¿Cuál es el precio de un activo financiero?. Este trabajo elabora sobre la primera pregunta. La segunda pregunta tiene que ver con el trabajo de Black, Scholes y Merton sobre la Valoración de opciones y derivados financieros, que también reciben el premio Nobel de Economía en 1997, apenas hace 20 años. Esta línea de trabajo donde se conjugan varias disciplinas como las Finanzas, Matemáticas, Estadística y más recientemente la Computación, genera un nuevo sector de Analistas Financieros Multidisciplinados que denominaremos Científicos de Datos Financieros.

La demanda principal a estos analistas consiste en partir de los datos y generar inferencias sobre los mismos aplicando la ciencia y la tecnología. Gran parte del trabajo de Markowitz ha sido la aplicación de métodos de optimización de portafolios con el uso de las computadoras y asistido por sistemas.

Las grandes burbujas y choques al mercado generan una gran preocupación sobre la gestión apropiada del riesgo y al mismo tiempo un entendimiento de la relación entre el riesgo que se asume y los beneficios posibles. El rendimiento es el resultado de una inversión después de un período de tiempo específico, podemos ganar o perder cuando realizamos una inversión, entonces el rendimiento es la variación (porcentual) del valor de un activo financiero en un período de tiempo. La variación del valor en términos absolutos es la ganancia o pérdida en valor monetario. Por otra parte el riesgo, en este caso, se puede definir como el grado de variabilidad de dicho rendimiento, esta es la medida propuesta por Markowitz y más recientemente han surgido nuevas medidas como el Valor en Riesgo (VaR) o el VaR Condicional (CVaR).

Usualmente para rendimientos iguales, en teoría, y de acuerdo a la teoría de decisiones racionales bajo incertidumbre, un inversionista debería preferir la inversión con menor riesgo y por otra parte para riesgos iguales, en general, deberíamos preferir la inversión de mayor rendimiento.

En este trabajo vamos a concentrarnos en la primera pregunta y nos basaremos en las aspiraciones del inversionista para conocer cuales instrumentos pueden formar parte de la cartera y en qué cantidad son requeridos para alcanzar un objetivo de inversión que combina la medida de rentabilidad y la medida de riesgo.

Los gerentes e inversionistas se enfrentan a situaciones donde deben escoger entre distintas alternativas de inversión basándose en estas dos variables principales, a saber, el rendimiento y el riesgo. Las preguntas son diversas y típicas como por ejemplo:

\begin{itemize}
\item ¿Cuál inversión es preferible? ¿Una con rendimiento promedio $8\%$ y riesgo (desviación estándar) de $4\%$ o una con $8\%$ y $3\%$ respectivamente?
\item ¿Cuál inversión es preferible? ¿Una con rendimiento promedio $8\%$ y riesgo (desviación estándar) de $4\%$ o una con $10\%$ y $4\%$ respectivamente?
\item ¿Cuál inversión es preferible? ¿Una con rendimiento promedio $8\%$ y riesgo (desviación estándar) de $4\%$ o una con $10\%$ y $5\%$ respectivamente?
\item ¿Cómo disminuyo el riesgo de una cartera de inversión, si deseo mantener el rendimiento?
\item ¿Cómo aumento la rentabilidad de una cartera de inversión, sin aumentar el riesgo?
\item ¿Cuál es al balance óptimo entre activos de inversión para lograr un máximo rendimiento al menor riesgo posible?
\end{itemize}

El problema fundamental es encontrar el balance adecuado entre un grupo de activos para alcanzar el máximo rendimiento con el mínimo riesgo.

Debido a los ciclos y choques que reciben los mercados, el mundo de hoy está retado a trabajar en un ambiente donde la incertidumbre sobre el desempeño de las inversiones es un hecho palpable y sin embargo desde el punto de vista de los inversionistas o accionistas es necesario ofrecer proyecciones de desempeño que si no se cumplen generan un cuestionamiento sobre el profesionalismo de los gerentes del portafolio o de productos.

Esto nos lleva a la valoración del riesgo debido a la incertidumbre inherente a los instrumentos de inversión, inclusive a nivel regulatorio la evaluación del riesgo con métodos más rigurosos ya empieza a ser necesario.

El trabajo de tesis consiste en la investigación documental del estado del arte en la conformación de portafolios de inversión y la aplicación de sus métodos, partiendo del trabajo de Harry Markowitz como base, con el objetivo de entender el desempeño de estas carteras en cada burbuja o choque que han recibido los mercado y además comparar algunas combinaciones de medidas de riesgo y rendimiento. Adicionalmente vamos a continuar la construcción de una aplicación Web interactiva para aplicar los métodos y valernos lo más posible de las diversas librerías construidas en R para este fin.




<!--chapter:end:010-introduction.Rmd-->

\mainmatter

# Introducción a la teoría de probabilidad

El mundo actual esta regido por metodologías que en su mayoría están estructuradas por hipótesis matemáticas, estas en la mayoría de los casos son fundamentales y de no cumplirse, la mayoría de los modelos pueden dejar de ser prácticos, por ejemplo, imaginase que usted quiera introducir un motor de un automóvil de la década de los sesenta en su automóvil moderno, con esto no queremos decir que no pueda funcionar, solo que estaría ignorando los principios mecánicos de su carro y con mucha seguridad provocaría fallas futuras. Igual pasa con los modelos de riesgo de crédito, por esto, dedicaremos este capítulo a dar un breve repaso, a la teoría de probabilidad y algún otro concepto matemático que sea necesario conocer.

## Espacio de probabilidad

Los espacios de probabilidad son las estructuras que se utilizan en matemáticas
para estudiar los fenómenos aleatorios, es decir, aquellos fenómenos que
tienen un alto grado de incertidumbre en sus resultados.

Los modelos de probabilidad trabajan con las "probabilidades" de los
diferentes resultados o sucesos que nos puedan interesar en un fenómeno
aleatorio, y nos dan respuestas más que satisfactorias en el estudio de dichos
fenómenos.

El enorme interés de la Teoría de la Probabilidad reside en que, aparte de tener unos preciosos desarrollos matemáticos, permite estudiar, a través de la Inferencia Estadística, los problemas que surgen en los estudios científicos que intentan obtener conclusiones a partir de datos reales.
 
\BeginKnitrBlock{definition}<div class="definition"><span class="definition" id="def:defi-espacio-muestral"><strong>(\#def:defi-espacio-muestral) </strong></span>El *espacio muestral*, $\Omega$, es el conjunto de todos los posibles resultados del fenómeno aleatorio que deseamos estudiar.</div>\EndKnitrBlock{definition}
 \
 
 
\BeginKnitrBlock{definition}<div class="definition"><span class="definition" id="def:defi-sigma-algebra"><strong>(\#def:defi-sigma-algebra) </strong></span>Una $\sigma$-*algebra* de sucesos, $S$, es un subconjunto de partes
de $\Omega$ que verifíca:

* Si $A \in S$ , entonces $A^c \in S$ ($c$ denota el coplemento de un conjunto).
  
* Si $A_1,A_2,...,A_n,...\in S$, entonces $\cup _n A_n \in S$</div>\EndKnitrBlock{definition}

La idea de una $\sigma$-algebra es que contenga todos los sucesos en los que
podemos estar interesados en un fenómenos aleatorio.
 
Ejemplo: Cuando el espacio muestral,$\Omega$, es fínito o numerable, la $\sigma$-algebra habitualmente utilizada es partes de $\Omega$. Es decir, consideramos que cualquier subconjunto de $\Omega$ puede ser un suceso de interés. Esta elección es la más sencilla y no ofrece ningún problema.

Con este par de definiciones podemos definir lo que es una medida de probabilidad.

\BeginKnitrBlock{definition}<div class="definition"><span class="definition" id="def:defi-probabilidad"><strong>(\#def:defi-probabilidad) </strong></span>Consideramos un espacio muestral $\Omega$ y una $\sigma$-algebra $S$. Una *función de probabilidad* o *medida de probabilidad* es una función $P : S \rightarrow [0, 1]$, que verifíca:

* $P(\Omega)=1$
  
* Si $A_1,A_2,...,A_n,...\in S$ son disjuntos, entonces: $$ P\big{(}\cup _n A_n\big{)}=\sum_n P(A_n)$$</div>\EndKnitrBlock{definition}

La manera más sencilla de entender el motivo de esta definición es pensar que el concepto de probabilidad trata de formalizar lo que ocurre con las frecuencias relativas de los distintos sucesos que podemos encontrarnos al lanzar un dado con 6 caras numeradas del 1 al 6:

* La frecuencia relativa de cualquier suceso siempre es un número entre 0 y 1.

* La frecuencia relativa de que salga algún número de los posibles (es decir, entre 1 y 6) es, evidentemente, 1.

* La frecuencia relativa de la unión de sucesos disjuntos es la suma de sus frecuencias relativas.

Esta forma de interpretar las medida de probabilidad no significa que esta sea la única posible, solo que a manera de ejemplo es la más fácil de explicar y visualizar.

 
\BeginKnitrBlock{definition}<div class="definition"><span class="definition" id="def:defi-esp-prob"><strong>(\#def:defi-esp-prob) </strong></span>Definimos como *espacio de probabilidad*  a la terna $( \Omega, S, P)$.</div>\EndKnitrBlock{definition}


## Variables aleatorias y función de distribución.

En este tema se tratará de formalizar numéricamente los resultados de un fenómeno aleatorio. Por tanto, una variable aleatoria es un valor numérico que corresponde a un resultado de un experimento aleatorio. Algunos ejemplos son: número de caras obtenidas al lanzar seis veces una moneda, número de llamadas que recibe un teléfono durante una hora, tiempo de fallo de una componente eléctrica, etc. 

###  Variable aleatoria

 
\BeginKnitrBlock{definition}<div class="definition"><span class="definition" id="def:defi-var-alea"><strong>(\#def:defi-var-alea) </strong></span>Dado un experimento aleatorio y asociado al mismo, un espacio de probabilidad $( \Omega, S, P)$, una variable aleatoria es una aplicación $Χ : \Omega → R$ , que a cada valor de $X$, del espacio muestral, le hace corresponder un número real. </div>\EndKnitrBlock{definition}

Ejemplo:  Consideramos un experimento aleatorio de lanzar una moneda al aire tres veces y anotamos el resultado. Se define la variable aleatoria X como número de caras aparecidas en los tres lanzamientos. 


### Tipos de variables aleatorias

Las variables aleatorias se clasifican en discretas y continuas: 

* *Discreta*: La variable aleatoria $X$ se dice que es discreta si los números asignados a los sucesos elementales de $\Omega$ son puntos aislados. Sus posibles valores constituyen un conjunto finito o infinito numerable. Por ejemplo, supongamos el experimento consistente en lanzar tres veces una moneda no trucada; si consideramos la variable aleatoria $X$="número de caras obtenidas en los tres lanzamientos"", los valores que puede tomar esta variable aleatoria son finitos "(0,1,2,3)". 

* *Continua*: La variable aleatoria $X$ será continua si los valores asignados pueden ser cualesquiera, dentro de ciertos intervalos, es decir, puede tomar cualquier valor real. Por ejemplo, si consideramos el experimento aleatoria consistente en medir el nivel de agua en un embalse y tomamos la variable aleatoria $X$="nivel de agua", esta puede tomar cualquier valor positivo.

### Distribución de probabilidad:

Es un modelo teórico que describe la forma en que varían los resultados de un experimento aleatorio, es decir, nos da todas las probabilidades de todos los posibles resultados que podrían obtenerse cuando se realiza un experimento aleatorio. Se clasifican como discretas o continuas. En la distribución de probabilidad discreta está permitido tomar sólo un número limitado de valores. En la continua, llamada función de densidad, la variable que se está considerando puede tomar cualquier valor dentro de un intervalo dado. 

#### Distribución de probabilidad discreta

Sea $X$ una variable aleatoria discreta que tomacomo posibles valores $x_1,x_2,.....x_n$, se define la distribución de probabilidad de X como el conjunto de pares (xi, pi) que a cada valor de la variable le asocia una probabilidad, donde $p_i= P(X=x_i)$, tal que la suma de todas las probabilidades es igual a uno.

Por ejemplo, en el experimento del dado la distribución de probabilidad biene dado por: (1;1/6),(2;1/6),(3;1/6),(4;1/6),(5;1/6),(6;1/6)


#### Distribución de probabilidad continua

Si la variable aleatoria es continua, hay infinitos valores posibles de la variable y entra cada dos de ellos se podrían definir infinitos valores. En estas condiciones no es posible deducir la probabilidad de un valor puntual de la variable como se puede hacer en el caso de las variables discretas. Pero sí es posible calcular la probabilidad acumulada hasta un cierto valor (función de distribución) y cómo cambia esa probabilidad acumulada en cada punto (densidad de probabilidad). Por tanto, cuando la variable aleatoria sea continua hablaremos de función de densidad. 


 
\BeginKnitrBlock{definition}<div class="definition"><span class="definition" id="def:defi-func-dens"><strong>(\#def:defi-func-dens) </strong></span>Sea $X$ una variable aleatoria continua, se llama función de densidad y se representa como $f(x)$ a una función no negativa definida sobre la recta real, tal que para cualquier intervalo $(a,b)$ que estudiemos se verifica: $$P(a\leq X \leq b) = \int_{a}^{b}  \! f(x) \, dx$$ </div>\EndKnitrBlock{definition}

### Función de distribución: 

La función de distribución describe el comportamiento probabilístico de una variable aleatoria X asociada a un experimento aleatorio y se representa como $F_x$. De igual forma estudiaremos el caso discreto y continuo.

#### Caso discreto

Sea $X$ una variable aleatoria discreta asociada a un espacio de probabilidad, se define la función de distribución: $$F_x:R\rightarrow [0,1] \quad \textrm{que verifica} \quad F_x(x)=P[X\leq x]=\sum_{x_i<x} P_i$$ 

#### Caso continuo

Sea $X$ una variable aleatoria continua con función de densidad $f(x)$, se define la función de distribución, $F_x$, como:$$F_x(x)=P[X \leq b ] = \int_{-\infty}^{b}  \! f(x) \, dx$$

### Algunos tipos de variables aleatorias

#### Bernoulli

Es un experimento en el cual solo existen dos eventos, en general exito(1) y fracaso(0), y ademas tiene asociada un parametro $p$ el cual es la probabilidad de exito, es decir, $$P(X=1)=p \quad \textrm{y} \quad P(X=0)=1-p$$

#### Binomial

La distribución de probabilidad discreta más usada es la distribución binomial  y consiste en :

* Repetimos un experimento de Bernoulli $n$ veces de forma identica

* Cada experimento es independiente de los anteriores

La variable aleatoria de Bernulli es la que expresa el número de éxitos, por lo tanto esta variable aleatoria tiene asociada dos parametros: un parametro $p$ que nos indica la probabilidad de éxito de cada evento, y un numero natural $n$ que nos indica la cantidad de veces que se realizó el experimento.

#### Poisson

Esta distribución es una de las más importantes distribuciones de variable discreta. Sus principales aplicaciones hacen referencia a la modelización de situaciones en las que nos interesa determinar el número de hechos de cierto tipo que se pueden producir en un intervalo de tiempo o de espacio, bajo presupuestos de aleatoriedad y ciertas circunstancias restrictivas. Otro de sus usos frecuentes es la consideración límite de procesos dicotómicos reiterados un gran número de veces si la probabilidad de obtener un éxito es muy pequeña .  

La distribución de Poisson tiene ajustado un parametros positivo $\lambda$ que representa el número de veces que se espera que ocurra el fenomeno durante un intervalo dado. La probabilidad de que un experimento de Poisson tenga $k$ ocurrencias es $$\frac{e^{-\lambda}\lambda^k}{k!}$$ 


#### Distribución uniforme

Es la distribución que sigue una variable aleatoria $X$ que toma valores en un intervalo $[a,b]$ con la misma probabilidad, su función de densidad es: $$f(x)= \left\{ \begin{array}{lcc}
             \frac{1}{b-a} &   \textrm{si}  & a \leq x \leq b \\
             \\ 0 &  \textrm{en otro caso} \\
             \end{array}
   \right.$$

#### Distribución exponencial

Es usada muchas veces para modelizar el comportamiento de variables aleatorias del tipo "tiempo transcurrido hasta el fallo de un componente industrial" o "el tiempo que se tarda en comletarse un proceso determinado". Depende de un parametro positivo $\lambda$, que esta relacionado con la media de ocurrecia de un evento. La función de densidad es:$$f(x)= \left\{ \begin{array}{lcc}
             0 &   \textrm{si}  & x \leq 0 \\
             \\ \lambda e^{-\lambda x} &  \textrm{si} & x>0 \\
             \end{array}
   \right.$$


#### Distribución normal

La distribución normal o gaussiana es muy importante puesto que se utiliza para modelar muchísimos fenómenos aleatorios; además incluso para aproximar otras distribuciones. La distribución normal aproxima lo observado en muchos procesos de medición sin errores sistemáticos, por ejemplo medida fisicas del cuerpo humano, medidas en la calidad de ciertos procesos industriales, entre otros. 

Esta distribución depende de dos parámetros: un parametro $\mu$ que es un indicador de la promedio del experimento que se esta estudiando y un parametro positivo $\sigma$ que es un indicador de la volatilidad del mismo. Su función de densidad es $$f(x)=\frac{1}{\sqrt{2\pi\sigma^2}}e^{-\frac{(x-\mu)^2}{2\sigma^2}}$$


### Esperanza de una variable aleatoria.

La esperanza matemática de una variable aleatoria $X$ es el número que expresa el valor medio del fenómeno que representa dicha variable.

#### Esperanza de una variable aleatoria discreta.

Dada una variable aleatoria $X$ que toma valores $x_1,x_2,x_3....x_n$ con distribución de probabilidad $P(X=x_i)=P_i$, se define la esperanza matemática de una variable aleatoria como: $$E[X]=\sum_{i=1}^{n}x_iP(X=x_i)=\sum_{i=1}^{n}x_iP_i$$

#### Esperanza de una variable aleatoria continua.

Sea $X$ una variable aleatoria continua con función de densidad $f(x)$, se define la esperanza matemática de esa variable aleatoria como:$$E[X]=\int_{-\infty}^{\infty}xf(x)dx$$

### Varianza de una variable aleatoria.

La varianza de una variable aleatoria es una característica numérica que proporciona una idea de la dispersión de la variable aleatoria respecto de su esperanza. Decimos que es un parámetro de dispersión y se define por:$$Var(X)=E((X-E(X))^2)=E(X^2)-(E(X))^2$$

### Covarianza de variables aleatorias.

La covarianza es un valor que indica el grado de variación conjunta de dos variables aleatorias. Es el dato básico para determinar si existe una dependencia entre ambas variables y además es el dato necesario para estimar otros parámetros básicos y se define como: Sean $X$, $Y$ variables aleatorias se define la covarianza entre $X$ y $Y$ como: $$Cov(X,Y)=E((X-E(X))(Y-E(Y)))$$





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

Para resolver estos inconvenientes hay modelos econométricos, generalmenteestimados por máxima verosimilitud, que tienen en cuenta la naturaleza discreta de la variable dependiente: se trata de los modelos de respuesta o elección binaria. Ellos utilizan ciertas funciones de distribución para la innovación, con el objetivo de limitar las probabilidades estimadas al intervalo [0,1]: las más usadas son la función de probabilidad acumulada normal estándar y la función logística. Cuando se usa la normal estándar se trata de un modelo probit, y logit cuando se usa la función logística. 


<!--chapter:end:300-Scoring.Rmd-->

\cleardoublepage 

# (APPENDIX) Apéndice {-}

# Software Tools

For those who are not familiar with software packages required for using R Markdown, we give a brief introduction to the installation and maintenance of these packages.

## R and R packages

R can be downloaded and installed from any CRAN (the Comprehensive R Archive Network) mirrors, e.g., https://cran.rstudio.com. Please note that there will be a few new releases of R every year, and you may want to upgrade R occasionally.

To install the **bookdown** package, you can type this in R:


```r
install.packages("bookdown")
```

This installs all required R packages. You can also choose to install all optional packages as well, if you do not care too much about whether these packages will actually be used to compile your book (such as **htmlwidgets**):


```r
install.packages("bookdown", dependencies = TRUE)
```

If you want to test the development version of **bookdown** on GitHub, you need to install **devtools** first:


```r
if (!requireNamespace('devtools')) install.packages('devtools')
devtools::install_github('rstudio/bookdown')
```

R packages are also often constantly updated on CRAN or GitHub, so you may want to update them once in a while:


```r
update.packages(ask = FALSE)
```

Although it is not required, the RStudio IDE can make a lot of things much easier when you work on R-related projects. The RStudio IDE can be downloaded from https://www.rstudio.com.

## Pandoc

An R Markdown document (`*.Rmd`) is first compiled to Markdown (`*.md`) through the **knitr** package, and then Markdown is compiled to other output formats (such as LaTeX or HTML) through Pandoc.\index{Pandoc} This process is automated by the **rmarkdown** package. You do not need to install **knitr** or **rmarkdown** separately, because they are the required packages of **bookdown** and will be automatically installed when you install **bookdown**. However, Pandoc is not an R package, so it will not be automatically installed when you install **bookdown**. You can follow the installation instructions on the Pandoc homepage (http://pandoc.org) to install Pandoc, but if you use the RStudio IDE, you do not really need to install Pandoc separately, because RStudio includes a copy of Pandoc. The Pandoc version number can be obtained via:


```r
rmarkdown::pandoc_version()
## [1] '1.19.2.1'
```

If you find this version too low and there are Pandoc features only in a later version, you can install the later version of Pandoc, and **rmarkdown** will call the newer version instead of its built-in version.

## LaTeX

LaTeX\index{LaTeX} is required only if you want to convert your book to PDF. The typical choice of the LaTeX distribution depends on your operating system. Windows users may consider MiKTeX (http://miktex.org), Mac OS X users can install MacTeX (http://www.tug.org/mactex/), and Linux users can install TeXLive (http://www.tug.org/texlive). See https://www.latex-project.org/get/ for more information about LaTeX and its installation.

Most LaTeX distributions provide a minimal/basic package and a full package. You can install the basic package if you have limited disk space and know how to install LaTeX packages later. The full package is often significantly larger in size, since it contains all LaTeX packages, and you are unlikely to run into the problem of missing packages in LaTeX.

LaTeX error messages may be obscure to beginners, but you may find solutions by searching for the error message online (you have good chances of ending up on [StackExchange](http://tex.stackexchange.com)). In fact, the LaTeX code converted from R Markdown should be safe enough and you should not frequently run into LaTeX problems unless you introduced raw LaTeX content in your Rmd documents. The most common LaTeX problem should be missing LaTeX packages, and the error may look like this:

```latex
! LaTeX Error: File `titling.sty' not found.

Type X to quit or <RETURN> to proceed,
or enter new name. (Default extension: sty)

Enter file name: 
! Emergency stop.
<read *> 
         
l.107 ^^M

pandoc: Error producing PDF
Error: pandoc document conversion failed with error 43
Execution halted
```

This means you used a package that contains `titling.sty`, but it was not installed. LaTeX package names are often the same as the `*.sty` filenames, so in this case, you can try to install the `titling` package. Both MiKTeX and MacTeX provide a graphical user interface to manage packages. You can find the MiKTeX package manager from the start menu, and MacTeX's package manager from the application "TeX Live Utility". Type the name of the package, or the filename to search for the package and install it. TeXLive may be a little trickier: if you use the pre-built TeXLive packages of your Linux distribution, you need to search in the package repository and your keywords may match other non-LaTeX packages. Personally, I find it frustrating to use the pre-built collections of packages on Linux, and much easier to install TeXLive from source, in which case you can manage packages using the `tlmgr` command. For example, you can search for `titling.sty` from the TeXLive package repository:

```bash
tlmgr search --global --file titling.sty
# titling:
#	 texmf-dist/tex/latex/titling/titling.sty
```

Once you have figured out the package name, you can install it by:

```bash
tlmgr install titling  # may require sudo
```

LaTeX distributions and packages are also updated from time to time, and you may consider updating them especially when you run into LaTeX problems. You can find out the version of your LaTeX distribution by:



```r
system('pdflatex --version')
## pdfTeX 3.14159265-2.6-1.40.18 (TeX Live 2017)
## kpathsea version 6.2.3
## Copyright 2017 Han The Thanh (pdfTeX) et al.
## There is NO warranty.  Redistribution of this software is
## covered by the terms of both the pdfTeX copyright and
## the Lesser GNU General Public License.
## For more information about these matters, see the file
## named COPYING and the pdfTeX source.
## Primary author of pdfTeX: Han The Thanh (pdfTeX) et al.
## Compiled with libpng 1.6.29; using libpng 1.6.29
## Compiled with zlib 1.2.11; using zlib 1.2.11
## Compiled with xpdf version 3.04
```

<!--chapter:end:400-apendice.Rmd-->

# Referencias {-}




<!--chapter:end:500-references.Rmd-->

