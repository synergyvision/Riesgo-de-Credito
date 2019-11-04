## Primero necesitamos las espocisiones al default

mydata <- read.csv("App/appTemplate/data//mydata1.csv")


reg <- read.csv("App/appTemplate/data/reg.csv")


###supondremos que son activos sin lineas extra 
### en este caso la exposicion coincide con el saldo

##exposicion de la cartera
#View(mydata)


EAD <- mydata[,6]

###supondremos que la perdida dado el default es la misma para toda  la cartera
### la institucion puede ajustar a un cliente en particular una perdida diferente

LGD <- 0.6

##ahora calcularemos las probabilidades de incumplimiento

s1 <- mydata



#s1[,1] <- replace(s1[,1], s1[,1]==1,-1)
#s1[,1] <- replace(s1[,1], s1[,1]==0,1)
#s1[,1] <- replace(s1[,1], s1[,1]==-1,0)




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
DP
####Calculamos ahora la perdida esperada

EL <- EAD*LGD*DP


### Se calcula la expocicion que se espera érder en caso de default

Ei <- EAD*LGD

#### Se escoge una unidad de perdida

E <- 300




###### se calculan las unidades de perdida

v <- Ei/E

e <- EL/E

###se calcula el paramtro de poisson
### correspondiente a la posibilidad de incumplimiento

lambda <- - log(1-DP)
lambda[10]

###creando las bandas

L <- ceiling(v)



bandas <- list()

for (k in 1:range(L)[2]) {
  
  
  bandas[[k]] <- which(L==k)
  
}


###se calculan los parametros de poisson por banda

lambdaj <- numeric(range(L)[2])

for (k in 1:range(L)[2]) {
  lambdaj[k] <- sum(lambda[bandas[[k]]])
}



#calculamos la perdida esperada por banda

ei <- lambdaj*1:length(bandas)

###factor de ajuste

gamm <- numeric(length(Ei)) 


for(i in 1:length(lambdaj)){
  
  
  gamm[bandas[[i]]] <- Ei[bandas[[i]]]/(i*E)
  
}
####Numero de incumplimientos de toda la cartera



length(lambdaj)
IncCar <- sum(lambdaj)



### Probabilides de unidades de perdida de toda la cartera

p0 <- exp(-IncCar)


probandas <- numeric(10000)

probandas[1] <- p0

probandasc <- probandas

length(ei)
eii <- numeric(10000)

eii[1:length(bandas)] <- ei[1:length(bandas)]
#View(eii)
for (i in 2:10000) {
  
  probandas[i] <- sum(probandasc[1:i-1]*rev(eii[1:i-1]))/(i-1)
  probandasc <- probandas
}

#View(probandas)ç

View(probandasc)
sum(probandas[1:10000])
  

plot(probandas)

4550*300
acum <- c()

for (l in 1:10000) {
  acum[l] <- sum(probandas[1:l]) 
}

View(acum)

#####Var



var <- min(which(acum > 0.95))*E

### Perdida esperada

saltos <- diff(acum)
pe <- saltos*1:9999
pe <-sum(pe)*E

pe
### tVar

c <- min(which(acum > 0.95))

v <- sum((saltos[c:length(saltos)]*c:9999))

pw <- 1-sum(saltos[1:c])
pw
tvar<- v/pw*E

data.frame(probandas)


ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()+geom_vline(xintercept =  2.45,show.legend = T)+geom_vline(xintercept =  4)


ser <- data.frame(probandas,(1:10000)*300)
ser
colnames(ser)<- c("prob","num")

as <- ggplot(ser, aes(x=num, y=prob)) + geom_point(color="yellow")+geom_vline(xintercept =  var)+geom_vline(xintercept =  pe)+geom_vline(xintercept =  tvar)
as

library(ggplot2)
library(plotly)


ggplotly(as)




# coef <- reduccion
# 
# coef <- coef[[1]]
# 
# coef <- as.data.frame(coef)
# 
# nom <- row.names(coef)
# nom[1] <- "Corte con el eje y"
# coe <- coef[,1]
# 
# coef <- data.frame(nom,coe) 
# colnames(coef) <- c("Variables","Coeficientes")
# coef
# 

library(ggplot2)
library(plotly)

d <- reduccion[[2]]
d

df <- data.frame(y = d)

p <- ggplot(df,aes(sample = y))
p + stat_qq() + stat_qq_line()


names(df)











inf <- data.frame(Criterios = c("Desviación","Desviación Nula","ACC"), Valor=c(reduccion[[10]],reduccion[[12]],reduccion[[11]]))


View(inf)
