library(plyr)
library("MASS")

##  modelo scoring

#comnjunto de datos

mydata <- read.csv("App/data/datos_completos.csv")
View(mydata)

##Eliminando algunas variables poco significativas
mydata <- mydata[ ,!colnames(mydata)=="X"]
mydata <- mydata[ ,!colnames(mydata)=="Telephone"]



##convirtiendo la data de interes a los valores clasicos
mydata$Creditability <- replace(mydata$Creditability, mydata$Creditability==1,-1)
mydata$Creditability <- replace(mydata$Creditability, mydata$Creditability==0,1)
mydata$Creditability <- replace(mydata$Creditability, mydata$Creditability==-1,0)

#####estableciendo los datos que son categoricos 

mydata$Account.Balance <- factor(mydata$Account.Balance)
mydata$Payment.Status.of.Previous.Credit <- factor(mydata$Payment.Status.of.Previous.Credit)
mydata$Purpose <- factor(mydata$Purpose)
mydata$Sex...Marital.Status <- factor(mydata$Sex...Marital.Status)
mydata$Guarantors <- factor(mydata$Guarantors)
mydata$Most.valuable.available.asset <- factor(mydata$Most.valuable.available.asset)
mydata$Concurrent.Credits <- factor(mydata$Concurrent.Credits)
mydata$Type.of.apartment <- factor(mydata$Type.of.apartment)
mydata$Occupation <- factor(mydata$Occupation)
mydata$Foreign.Worker <- factor(mydata$Foreign.Worker)


#construccion del modelo, se uso probit, pero en general los mas usados son logit y probit

probit <- glm(Creditability ~. , data = mydata, family = binomial(link = "probit"))


##Reduciendo las variables con backward, step usa por defecto backward

backwards = step(probit)

###el score, entre menor menos probabilidad de incumplimiento y entre mayor
### aumenta la probabilidad de incumplimiento.


Score <- backwards$linear.predictors
head(Score)

##probabilidades de incumplimiento de los clientes

PD <- probit$fitted.values
#head(PD)


View(data.frame(Score,PD))


###control con malos perfiles


one <- which(mydata$Creditability==1)

c1 <- probit$linear.predictors[one]
c2 <- probit$fitted.values[one]


###si establecemos el corte en 0.5
bienCali1 <- length(which(c1 > 0.5))/300
bienCali1

###si establecemos el corte en 0.3
bienCali1 <- length(which(c1 > 0.3))/300
bienCali1

###si establecemos el corte en 0.1
bienCali1 <- length(which(c1 > 0))/300
bienCali1


###control con buenos perfiles


one <- which(mydata$Creditability==0)

c1 <- probit$linear.predictors[one]
c2 <- probit$fitted.values[one]


###si establecemos el corte en 0.5
bienCali0 <- length(which(c1 < 0.5))/700
bienCali0

###si establecemos el corte en 0.3
bienCali0 <- length(which(c1 < 0.3))/700
bienCali0

###si establecemos el corte en 0.1
bienCali0 <- length(which(c1 < 0.1))/700
bienCali0






?boxplot

boxplot(mydata$No.of.dependents~ factor(Creditability), data = mydata)


mydata$Creditability <- factor(mydata$Creditability)

d <- mydata$Account.Balance

boxplot(s~d)
