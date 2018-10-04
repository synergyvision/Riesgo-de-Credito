library(fastDummies)
library(readr)

datos <- read_csv("~/Riesgo_de_Credito/App/appTemplate/data/datos_completos.csv")


D<- datos
M <- c()


for (i in 1:length(names(datos))) {
  
  if(summary(as.factor(D[[i]]))<=10){
    M[length(M)+1] <- i
    
  }
  
}


D1 <- D[,-M]


pval <- NULL
nomb <- colnames(D1)

for (i in 2:length(colnames(D1))) {
  
  df <- D1[,c(1,i)]
  df1 <- dummy_cols(df,select_columns = nomb[i])
  
  d0 <- subset(df1,Creditability==0)
  d1 <- subset(df1,Creditability==1)
  
  d0 <- apply(d0, 2, sum)
  d1 <- apply(d1, 2, sum)
  
  d <- data.frame(t(data.frame(d0,d1)))
  
  d <-d[,-2]
  
  
  d$Creditability[2] <- 1
  
  d$Creditability[2] <- "buenos"
  d$Creditability[1] <- "malos"
  
  
  nombre <- d$Creditability
  rownames(d) <- nombre
  d <- d[,-1]
  
  pr <- chisq.test(d)
  pval[i]<-pr$p.value
  
  
}

View(df1)

pval <- t(pval)
j

vd <- nomb[which(pval > 0.05)]


j <- colnames(datos)


inf <- data.frame(pval)
colnames(inf)<-nomb

View(inf)

final <- datos[, !(j %in% vd)]
names(final)

