##convirtiendo la data de interes a los valores clasicos
mydata <- read.csv("App/data/datos_completos.csv")

mydata$Creditability <- replace(mydata$Creditability, mydata$Creditability==1,-1)
mydata$Creditability <- replace(mydata$Creditability, mydata$Creditability==0,1)
mydata$Creditability <- replace(mydata$Creditability, mydata$Creditability==-1,0)
###

####

mydata$Account.Balance <- factor(mydata$Account.Balance)
dummies = model.matrix(~mydata$Account.Balance)
dummies <- dummies[,-1]
mydata <- mydata[,!colnames(mydata)=="Account.Balance"]
mydata <- cbind(mydata,dummies)
#View(dummies)

mydata$Payment.Status.of.Previous.Credit <- factor(mydata$Payment.Status.of.Previous.Credit)
dummies = model.matrix(~mydata$Payment.Status.of.Previous.Credit)
dummies <- dummies[,-1]
mydata <- mydata[,!colnames(mydata)=="Payment.Status.of.Previous.Credit"]
mydata <- cbind(mydata,dummies)
#View(dummies)



mydata$Purpose <- factor(mydata$Purpose)
dummies = model.matrix(~mydata$Purpose)
dummies <- dummies[,-1]
mydata <- mydata[,!colnames(mydata)=="Purpose"]
mydata <- cbind(mydata,dummies)
#View(dummies)

mydata$Sex...Marital.Status <- factor(mydata$Sex...Marital.Status)
dummies = model.matrix(~mydata$Sex...Marital.Status)
dummies <- dummies[,-1]
mydata <- mydata[,!colnames(mydata)=="Sex...Marital.Status"]
mydata <- cbind(mydata,dummies)
#View(dummies)

###train and test
ceros <- subset(mydata, Creditability==0)
unos <- subset(mydata, Creditability==1)

nrow(ceros)
indices0 <- sample( 1:nrow( ceros ), 400 )
ceros.muestreado <- ceros[ indices0, ]
ceros.test <- ceros[-indices0,]



indices1 <- sample( 1:nrow( unos ), 200 )
unos.muestreado <- unos[ indices1, ]
unos.test <- unos[-indices1,]


train <- rbind(ceros.muestreado,unos.muestreado)
test <- rbind(ceros.test,unos.test)

dim(train)
dim(test)


train <- train[c("Creditability","mydata$Account.Balance2","mydata$Account.Balance3","mydata$Account.Balance4","Duration.of.Credit..month.","mydata$Payment.Status.of.Previous.Credit1","mydata$Payment.Status.of.Previous.Credit2","mydata$Payment.Status.of.Previous.Credit3","mydata$Payment.Status.of.Previous.Credit4","mydata$Purpose1","mydata$Purpose2","mydata$Purpose3","mydata$Purpose4","mydata$Purpose5","mydata$Purpose6","mydata$Purpose8","mydata$Purpose9","mydata$Purpose10","Credit.Amount","Value.Savings.Stocks","Length.of.current.employment","Instalment.per.cent","mydata$Sex...Marital.Status2","mydata$Sex...Marital.Status3","mydata$Sex...Marital.Status4","Foreign.Worker","Duration.in.Current.address")]
x_train <- train[,-1]
y_train <- train$Creditability



test  <- test[c("Creditability","mydata$Account.Balance2","mydata$Account.Balance3","mydata$Account.Balance4","Duration.of.Credit..month.","mydata$Payment.Status.of.Previous.Credit1","mydata$Payment.Status.of.Previous.Credit2","mydata$Payment.Status.of.Previous.Credit3","mydata$Payment.Status.of.Previous.Credit4","mydata$Purpose1","mydata$Purpose2","mydata$Purpose3","mydata$Purpose4","mydata$Purpose5","mydata$Purpose6","mydata$Purpose8","mydata$Purpose9","mydata$Purpose10","Credit.Amount","Value.Savings.Stocks","Length.of.current.employment","Instalment.per.cent","mydata$Sex...Marital.Status2","mydata$Sex...Marital.Status3","mydata$Sex...Marital.Status4","Foreign.Worker","Duration.in.Current.address")]
x_test <- test[,-1]
y_test <- test$Creditability


dim(test)



accuracy = function(actual, predicted) {
  mean(actual == predicted)
}

##escoger K

set.seed(42)
k_to_try = 1:30

acc_k = rep(x = 0, times = length(k_to_try))

for(i in seq_along(k_to_try)) {
  pred = knn(train = x_train, 
             test = x_test, 
             cl = y_train, 
             k = k_to_try[i])
  acc_k[i] = accuracy(y_test, pred)
}

#graficando para escoger K

# plot accuracy vs choice of k
plot(acc_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "classification accuracy",
     main = "Accuracy vs Neighbors")
# add lines indicating k with best accuracy
abline(v = which(acc_k == max(acc_k)), col = "darkorange", lwd = 1.5)
# add line for max accuracy seen
abline(h = max(acc_k), col = "grey", lty = 2)
# add line for prevalence in test set
abline(h = mean(y_test == "No"), col = "grey", lty = 2)

# extraemos el atributo Probabilidad

pred = knn(train = x_train, 
           test = x_test, 
           cl = y_train, 
           k = 23 ,prob = T)

accuracy(y_test, pred)

probknn <- attributes(pred)[["prob"]]


View(probknn)
