library(plyr)
library("MASS")

##  modelo scoring

#comnjunto de datos

mydata <- read.csv("App/data/datos_completos.csv")
View(mydata)
attach(mydata)

##convirtiendo la data de interes a los valores clasicos
mydata[,1] <- replace(mydata[,1], mydata[,1]==1,-1)
mydata[,1] <- replace(mydata[,1], mydata[,1]==0,1)
mydata[,1] <- replace(mydata[,1], mydata[,1]==-1,0)



#####estableciendo los datos que son categoricos 

mydata$Account.Balance <- factor(mydata$Account.Balance)
mydata$Payment.Status.of.Previous.Credit <- factor(mydata$Payment.Status.of.Previous.Credit)
mydata$Purpose <- factor(mydata$Purpose)
mydata$Value.Savings.Stocks <- factor(mydata$Value.Savings.Stocks)
mydata$Length.of.current.employment <- factor(mydata$Length.of.current.employment)
mydata$Instalment.per.cent <- factor(mydata$Instalment.per.cent)
mydata$Sex...Marital.Status <- factor(mydata$Sex...Marital.Status)
mydata$Guarantors <- factor(mydata$Guarantors)
mydata$Duration.in.Current.address <- factor(mydata$Duration.in.Current.address)
mydata$Concurrent.Credits <- factor(mydata$Concurrent.Credits)
mydata$Type.of.apartment <- factor(mydata$Type.of.apartment)
mydata$No.of.Credits.at.this.Bank <- factor(mydata$No.of.Credits.at.this.Bank)
mydata$Occupation <- factor(mydata$Occupation)
mydata$No.of.dependents <- factor(mydata$No.of.dependents)
mydata$Telephone <- factor(mydata$Telephone)
mydata$Foreign.Worker <- factor(mydata$Foreign.Worker)




##Eliminando algunas variables poco significativas
mydata <- mydata[ ,!colnames(mydata)=="X"]


#construccion del modelo, se uso probit, pero en general los mas usados son logit y probit

modelo <- glm(Creditability ~. , data = mydata, family = binomial(link = "probit"))

##Reduciendo las variables con backward, step usa por defecto backward

reduccion = step(modelo)

###el score, entre menor menos probabilidad de incumplimiento y entre mayor
### aumenta la probabilidad de incumplimiento.


Score <- reduccion$linear.predictors
#head(Score)

##probabilidades de incumplimiento de los clientes

PD <- reduccion$fitted.values
#head(PD)


View(data.frame(Score,PD))

###control con malos perfiles


one <- which(mydata$Creditability==1)

c1 <- reduccion$linear.predictors[one]
c2 <- reduccion$fitted.values[one]


###si establecemos el corte en 0.5
bienCali1 <- length(which(c1 > 0.5))/300
bienCali1

###si establecemos el corte en 0.3
bienCali1 <- length(which(c1 > 0.3))/300
bienCali1

###si establecemos el corte en 0
bienCali1 <- length(which(c1 > 0))/300
bienCali1




###control con buenos perfiles


ceros <- which(mydata$Creditability==0)

c1 <- reduccion$linear.predictors[ceros]
c2 <- reduccion$fitted.values[ceros]


###si establecemos el corte en 0.5
bienCali0 <- length(which(c1 < 0.5))/700
bienCali0

###si establecemos el corte en 0.3
bienCali0 <- length(which(c1 < 0.3))/700
bienCali0

###si establecemos el corte en 0
bienCali0 <- length(which(c1 < 0))/700
bienCali0

######Variables del modelo

(names(reduccion$coefficients))



################ estudio de independencia entre variables cualitativas 

##caso en que no pasa la prueba, es decir, es posible que se elimine esta variable
V1 <- mydata$Creditability
V2 <- mydata$Telephone

tabla <- table(V1,V2)
tabla
prueba <- chisq.test(tabla)
pvalor <- prueba$p.value
pvalor


###Al realizar la reducción de variables con backward
###se estudiaran algunas variables cualitativas que puden ser excluidas

##caso que si la pasa, es decir, es posible que se incluya esta variable

V1 <- mydata$Creditability
V2 <- mydata$Foreign.Worker

tabla <- table(V1,V2)
tabla
prueba <- chisq.test(tabla)
pvalor <- prueba$p.value
pvalor

########Corriendo el modelo luego de eliminar variable guarantor

modelo <- glm(Creditability ~Account.Balance+Duration.of.Credit..month.
              +Payment.Status.of.Previous.Credit+Purpose+Credit.Amount
              +Value.Savings.Stocks+Length.of.current.employment
              +Instalment.per.cent+Sex...Marital.Status
              +Foreign.Worker+Duration.in.Current.address, data = mydata, family = binomial(link = "probit"))

###control con malos perfiles


one <- which(mydata$Creditability==1)

c1 <- modelo$linear.predictors[one]
c2 <- modelo$fitted.values[one]


###si establecemos el corte en 0.5
bienCali1 <- length(which(c1 > 0.5))/300
bienCali1

###si establecemos el corte en 0.3
bienCali1 <- length(which(c1 > 0.3))/300
bienCali1

###si establecemos el corte en 0
bienCali1 <- length(which(c1 > 0))/300
bienCali1




###control con buenos perfiles


ceros <- which(mydata$Creditability==0)

c1 <- reduccion$linear.predictors[ceros]
c2 <- reduccion$fitted.values[ceros]


###si establecemos el corte en 0.5
bienCali0 <- length(which(c1 < 0.5))/700
bienCali0

###si establecemos el corte en 0.3
bienCali0 <- length(which(c1 < 0.3))/700
bienCali0

###si establecemos el corte en 0
bienCali0 <- length(which(c1 < 0))/700
bienCali0
################ estudio de independencia entre variables cuantitativas

attach(mydata)

df0 <- subset(mydata,Creditability==0 )
df1 <- subset(mydata,Creditability==1 )

#####Posible variable a incluir debido a su diferencia entre factores
media0 <- mean(df0$Duration.of.Credit..month.)
sd0 <- sd(df0$Duration.of.Credit..month.)
median0 <- median(df0$Duration.of.Credit..month.)

media0
sd0
median0 

media1 <- mean(df1$Duration.of.Credit..month.)
sd1 <- sd(df1$Duration.of.Credit..month.)
median1 <- median(df1$Duration.of.Credit..month.)

media1
sd1
median1

boxplot(Credit.Amount~Creditability, data = mydata)



###########Posible variable a no incluir debido a su poca diferencia entre factores
media0 <- mean(df0$Age..years.)
sd0 <- sd(df0$Age..years.)
median0 <- median(df0$Age..years.)
media0
sd0
median0 

media1 <- mean(df1$Age..years.)
sd1 <- sd(df1$Age..years.)
median1 <- median(df1$Age..years.)
media1
sd1
median1






##############Analisis de la data con una subpoblacion


ceros <- subset(mydata, Creditability==0)
unos <- subset(mydata, Creditability==1)


indices <- sample( 1:nrow( ceros ), 200 )
ceros.muestreado <- ceros[ indices, ]


indices <- sample( 1:nrow( unos ), 200 )
unos.muestreado <- unos[ indices, ]
View(unos.muestreado)

train <- rbind(ceros.muestreado,unos.muestreado)
dim(train)

modelo <- glm(Creditability ~Account.Balance+Duration.of.Credit..month.
              +Payment.Status.of.Previous.Credit+Purpose+Credit.Amount
              +Value.Savings.Stocks+Length.of.current.employment
              +Instalment.per.cent+Sex...Marital.Status
              +Foreign.Worker+Duration.in.Current.address, data = train, family = binomial(link = "probit"))
View(modelo$fitted.values)


#head(Score)

###control con malos perfiles


one <- which(train$Creditability==1)

c1 <- modelo$linear.predictors[one]
c2 <- modelo$fitted.values[one]


###si establecemos el corte en 0.5
bienCali1 <- length(which(c1 > 0.5))/200
bienCali1

###si establecemos el corte en 0.3
bienCali1 <- length(which(c1 > 0.3))/200
bienCali1

###si establecemos el corte en 0
bienCali1 <- length(which(c1 > 0))/200
bienCali1




###control con buenos perfiles


ceros <- which(train$Creditability==0)

c1 <- modelo$linear.predictors[ceros]
c2 <- modelo$fitted.values[ceros]


###si establecemos el corte en 0.5
bienCali0 <- length(which(c1 < 0.5))/200
bienCali0

###si establecemos el corte en 0.3
bienCali0 <- length(which(c1 < 0.3))/200
bienCali0

###si establecemos el corte en 0
bienCali0 <- length(which(c1 < 0))/200
bienCali0
