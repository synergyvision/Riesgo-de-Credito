library(plyr)
library("MASS")

##  scoring modelo probit

#comnjunto de datos de prueba.

mydata <- read.csv("datos_completos.csv")
#View(mydata)
mydata <- mydata[ ,!colnames(mydata)=="X"]
mydata <- mydata[ ,!colnames(mydata)=="Telephone"]

mydata$Creditability <- replace(mydata$Creditability, mydata$Creditability==1,-1)
mydata$Creditability <- replace(mydata$Creditability, mydata$Creditability==0,1)
mydata$Creditability <- replace(mydata$Creditability, mydata$Creditability==-1,0)

#####estableciendo los datos categoricos

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


#construccion del modelo 

probit <- glm(Creditability ~. , data = mydata, family = binomial(link = "probit"))


##Reduciendo las variables con backward

backwards = step(probit)

backwards$fitted.values[340]

backwards$linear.predictors

attach(probit)

coefficients

co <- c(-2.386836312,0.001375591,0.477730048,-0.812138076)

val <- c(1,380,3.61,3)

s <- sum(co*val)

probit$fitted.values


## confidence intervals
confint(probit)

probit$fitted.values


probit$effects


??glm

qnorm(s)

View(mydata)

?glm


-3.052e-01
