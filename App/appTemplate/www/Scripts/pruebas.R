s1 <-  read.csv("~/Riesgo-de-Credito/App/data/datos_completos.csv")

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

library(pROC)

l <- roc(train$dependiente  ~ modelo$fitted.values)
ggplot(l)


# Example from http://sachsmc.github.io/plotROC/
library(ggplot2)
library(plotROC)
library(plotly)

# I start by creating an example data set. There are 2 markers, 
# one that is moderately predictive and one that is not as predictive.

set.seed(2529)
D.ex <- rbinom(200, size = 1, prob = .5)
M1 <- rnorm(200, mean = D.ex, sd = .65)
M2 <- rnorm(200, mean = D.ex, sd = 1.5)

test <- data.frame(D = D.ex, D.str = c("Healthy", "Ill")[D.ex + 1], 
                   M1 = M1, M2 = M2, stringsAsFactors = FALSE)

# The Roc Geom
# Next I use the ggplot function to define the aesthetics, and 
# the geom_roc function to add an ROC curve layer. 

basicplot <- ggplot(test, aes(d = D, m = M1)) + geom_roc()

basicplot


if(requireNamespace("plotly", quietly = TRUE)) {
  set.seed(34903490)
  x = rnorm(50)
  y = 0.5*x^2 + 2*x + rnorm(length(x))
  frm = data.frame(x=x,yC=y>=as.numeric(quantile(y,probs=0.8)))
  plotlyROC(frm, 'x', 'yC', TRUE, 'example plot', estimate_sig = TRUE)
}







reduccion = step(modelo)

n <- test[,-1]

pdata <- predict(reduccion, newdata = n, type = "response")

pdata

pred <- confusionMatrix(data = as.factor(as.numeric(pdata>0.7)), reference = as.factor(test$dependiente))


conf <- pred$table
c1 <- c("Prediccion",0,1)
c2 <- c(0,conf[1,1],conf[1,2])
c3 <- c(1,conf[2,1],conf[2,2])

cbind(c1,c2,c3)


l <- roc(train$dependiente  ~ reduccion$fitted.values)
plot(l,legacy.axes=T)



Score <- predict(reduccion, newdata = s1, type = "link")
PD <- predict(reduccion, newdata = s1, type = "response")
d <- cbind(Score,PD)

eve
View(d)
length(s1[,1])




library(isdals)
data(bodyfat)
attach(bodyfat)
par(mfrow=c(1,2))
plot( bodyfat )



attach(attitude);
rating.hat = predict( lm( rating ~ learning) )
complaints.hat = predict( lm( complaints~learning) )
#### place your code on the next line ####


pcor( attitude ) 
