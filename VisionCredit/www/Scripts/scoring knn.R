###########KNN

library(ISLR)
library(class)
library(MASS)

###uN EJEMPLO

set.seed(42)
Default$student = as.numeric(Default$student) - 1
default_index = sample(nrow(Default), 5000)
default_train = Default[default_index, ]
default_test = Default[-default_index, ]

# training data
X_default_train = default_train[, -1]
y_default_train = default_train$default

# testing data
X_default_test = default_test[, -1]
y_default_test = default_test$default


###para construir el modelo

modelo <- knn(train = X_default_train, 
         test = X_default_test, 
         cl = y_default_train, 
         k = 11 ,prob = T)

#train, the predictors for the train set.
#test, the predictors for the test set. knn() will output results for these cases.
#cl, the true class labels for the train set.
#k, the number of neighbors to consider.

## funcion para medir que tan bueno es le modelo

accuracy = function(actual, predicted) {
  mean(actual == predicted)
}

###Usando la función
accuracy(actual = y_default_test,
         predicted = knn(train = X_default_train, 
                         test = X_default_test, 
                         cl = y_default_train, k = 11))

## en general es mejor llevar todas las variables a una misma escala

accuracy(actual = y_default_test,
         predicted = knn(train = scale(X_default_train), 
                         test = scale(X_default_test), 
                         cl = y_default_train, k = 11))
###escoger K

set.seed(42)
k_to_try = 1:100
acc_k = rep(x = 0, times = length(k_to_try))

for(i in seq_along(k_to_try)) {
  pred = knn(train = scale(X_default_train), 
             test = scale(X_default_test), 
             cl = y_default_train, 
             k = k_to_try[i])
  acc_k[i] = accuracy(y_default_test, pred)
}


##graficando para escoger K

# plot accuracy vs choice of k
plot(acc_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "classification accuracy",
     main = "Accuracy vs Neighbors")
# add lines indicating k with best accuracy
abline(v = which(acc_k == max(acc_k)), col = "darkorange", lwd = 1.5)
# add line for max accuracy seen
abline(h = max(acc_k), col = "grey", lty = 2)
# add line for prevalence in test set
abline(h = mean(y_default_test == "No"), col = "grey", lty = 2)

# extraemos el atributo Probabilidad

probknn <- attributes(modelo)[3]

probknn$prob[428]
