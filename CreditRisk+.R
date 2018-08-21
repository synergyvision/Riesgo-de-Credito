### Primero necesitamos las espocisiones al default

mydata <- read.csv("App/appTemplate/data/datos_completos.csv")

###supondremos que son activos sin lineas extra 
### en este caso la exposicion coincide con el saldo

##exposicion de la cartera
View(mydata)


EAD <- mydata[,6]

###supondremos que la perdida dado el default es la misma para toda  la cartera
### la institucion puede ajustar a un cliente en particular una perdida diferente

LGD <- 0.6

##ahora calcularemos las probabilidades de incumplimiento

s1 <- mydata


  
  s1[,1] <- replace(s1[,1], s1[,1]==1,-1)
  s1[,1] <- replace(s1[,1], s1[,1]==0,1)
  s1[,1] <- replace(s1[,1], s1[,1]==-1,0)




ceros <- subset(s1, s1[,1]==0)
unos <- subset(s1, s1[,1]==1)


indices0 <- sample( 1:nrow( ceros ), nrow(ceros)*0.7 )
ceros.muestreado <- ceros[ indices0, ]
ceros.test <- ceros[-indices0,]

indices1 <- sample( 1:nrow( unos ), nrow(unos)*0.7 )
unos.muestreado <- unos[ indices1, ]
unos.test <- unos[-indices1,]

train <- rbind(ceros.muestreado,unos.muestreado)
test <- rbind(ceros.test,unos.test)

colnames(train)[1] <- "dependiente"
colnames(test)[1] <- "dependiente"

modelo <- glm(dependiente ~. , data = train, family = binomial(link = "probit"))


reduccion = step(modelo)

Score <- predict(reduccion, newdata = s1, type = "link")



DP <- predict(reduccion, newdata = s1, type = "response")

####Calculamos ahora la perdida esperada

EL <- EAD*LGD*DP


### Se calcula la expocicion que se espera érder en caso de default

Ei <- EAD*LGD

#### Se escoge una unidad de perdida

E <- 200 




###### se calculan las unidades de perdida

v <- Ei/E

e <- EL/E

###se calcula el paramtro de poisson
### correspondiente a la posibilidad de incumplimiento

lambda <- - log(1-DP)


###creando las bandas

L <- ceiling(v)



bandas <- list()

for (k in 1:range(L)[2]) {
  
  
  bandas[[k]] <- which(L==k)
  
}


bandas






























