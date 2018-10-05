
library(readr)

datos <- read_csv("~/Riesgo_de_Credito/App/appTemplate/data/datos_completos.csv")


D<- datos
M <- c()


for (i in 1:length(names(datos))) {
  
  if(summary(as.factor(D[[i]]))<=10){
    M[length(M)+1] <- i
    
  }
  
}
M
D1 <- D[,c(1,M)]
D1
pval <- NULL

nomb <- colnames(D1)


for (i in 2:length(nomb)) {

  df1 <- D1[,c(1,i)]

  d0 <- subset(df1, Creditability==0)
  d1 <- subset(df1, Creditability==1)
  
   p1 <- d0[[2]]
   p2 <- d1[[2]]


  w <-  ks.test(p1,p2)

  pval[i] <- w$p.value

}

pval <- t(pval)


vd <- nomb[which(pval > 0.05)]



j <- colnames(datos)


inf <- data.frame(pval)
colnames(inf)<-nomb



final <- datos[, !(j %in% vd)]






