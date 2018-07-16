library(plyr)
library("MASS")

##  modelo scoring

#comnjunto de datos

mydata <- read.csv("datos_completos.csv")
#View(mydata)

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
