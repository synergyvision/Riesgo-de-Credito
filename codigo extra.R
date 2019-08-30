x <- c(A = 20, B = 15, C = 25)
chisq.test(x)


pval <- function(datos){
  
  
  D<- datos
  M <- c()
  
  
  for (i in 2:length(names(datos))) {
    
    if(length(summary(as.factor(D[[i]])))<=10){
      M[length(M)+1] <- i
      
    }
    
  }
  
  
  D1 <- D[,c(1,M)]
  
  
  pval <- NULL
  est <- NULL
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
    pval[i] <- round(pr$p.value,4)
    est[i] <-round(pr$statistic,4)
    
  }
  
  
  pval <- t(pval)
  est <- t(est)
  
  vd <- nomb[which(pval > 0.05)]
  
  
  j <- colnames(datos)
  
  
  
  inf <- rbind(pval,est)
  inf[1,1] <- "P-valor"
  inf[2,1] <- "Estadistico"
  colnames(inf) <- nomb
  return(inf)
}


l = pval(mydata1)
View(l)


iris[1,]



x <- c(A = 20, B = 13, C = 25)
d <- chisq.test(x)




dchisq(0.5,2)

D<- mydata
nomb <- colnames(D)
D1 <- D[,c(1,9)]
D1
df1 <- dummy_cols(D1,select_columns = nomb[9])

d0 <- subset(df1,Creditability==0)
d1 <- subset(df1,Creditability==1)

d0 <- apply(d0, 2, sum)
d1 <- apply(d1, 2, sum)

d <- data.frame(t(data.frame(d0,d1)))

d <-d[,-2]


d$Creditability[2] <- 1

d$Creditability[2] <- "buenos"
d$Creditability[1] <- "malos"
d

nombre <- d$Creditability
rownames(d) <- nombre
d <- d[,-1]
d
pr <- chisq.test(d)
pr


dchisq(3,5.4768)

dchisq(3,5.4768)


qchisq(0.95,3)










x <- rnorm(50)
y <- rnorm(30)
# Do x and y come from the same distribution?
l <- ks.test(x, y)
l$data.name
l













j <- glm(Sepal.Length~.,data=iris)

l1 <-j$coefficients
l2 <-names(j$coefficients)


View(t(rbind(l2,l1)))

class(j)

j$deviance
j$null.deviance
j$aic
j$iter



summary(j)




data.frame(c(1,2),c("a","b"))












pd <- c(runif(100,0,0.05),runif(100,0.05,0.1),runif(100,0.1,0.2),runif(100,0.2,0.3),runif(100,0.3,1))

r <- c(rep("a",100),rep("b",100), rep("c",100), rep("d",100), rep("e",100) )

dat <- cbind(pd,r)
#write.csv(dat, file="rat.csv", row.names = T)


colnames(dat)<- c("PD","rat")
View(dat)



ind <- as.data.frame(as.numeric(dat[,1]))
colnames(ind)<- c("PD")
d <- lda(dat[,2]~ .,data = ind)


df <- as.data.frame(c(0.001,0.2,0.09,0.5))
colnames(df) <- c("PD")

d

p <- predict(d,df)

as.data.frame(p$class)







lda_mo <- function(dat){
  
ind <- as.data.frame(as.numeric(dat[,1]))
colnames(ind)<- c("PD") 
  
d <- lda(dat[,2]~ .,data = ind)
return(d)
  
}

 
infLda <- function(lda){
  
  prior <- round(f$prior,4)
  mea <- t(round(f$means,4))
  inf <- rbind(prior,mea)
  inf <- cbind(c("Probabilidades a Priori","Valores medio de los grupos"),inf)
  return(inf)
}
  
  
  
  
f <- lda_mo(dat)





df <- as.data.frame(c(0.001,0.2,0.09,0.5))
df[,1]
colnames(df) <- c("PD")



predict(f,df)





Rat_Cli <- function(d,df){
  
  pd <- as.data.frame(as.numeric(df[,1]))
  colnames(pd) <- c("PD")
  rat <- predict(d,pd)
  rat_cli <- as.data.frame(p$class)
  colnames(rat_cli) <- c("Rating")
  return(rat_cli)
  
}

Rat_Cli(d,df)




hist(rnorm(mean = 0.75,sd = 0.5,n = 100))


perdidas <- rnorm(mean = 0.75,sd = 0.05,n = 10000)
write.csv(perdidas,row.names = F,col.names = T,"perdidas.csv")



perdidas <- read.csv("App/data/perdidas.csv",header = T)
#View(perdidas)

medias <- NULL

for ( i  in 1:100) {
  muestra <- sample(perdidas$Perdidas,ceiling(0.05*length(perdidas$Perdidas)),replace = T)
  
  medias[i] <- mean(muestra)
  
}

medias







p7 <- ggplot(perdidas, aes(x = Perdidas)) +
  geom_histogram()+labs(x = "Pérdida",y = "Frecuencia")


ggplotly(p7)


 p7+ labs(x = "Pérdida",y = "Frecuencia")




library(plotly)




ggplotly



d <- as.data.frame(iris$Species)

colnames(d) <- c("Perdidas")





##############

cla <- read.csv("~/Riesgo_de_Credito/App/data/Historica_de_perdidas.csv", sep=";")



bostCL <- function(cla, n ,tam){
  
  clases <- names(table(cla["clases"]))
  
  medias <- NULL
  desviacion <- NULL
  
 
  
   for (i in 1:length(clases)) {
    
    pos <- which(cla[,1]==clases[i])
    
    filtro <- cla[pos,2]
    
    mediabost <- NULL
        for ( j  in 1:n) {
          muestra <- sample(filtro,ceiling((tam/100)*length(filtro)),replace = T)
          mediabost[j] <- mean(muestra)
        }
    
    medias[i] <- mean(mediabost)
    desviacion[i] <- sd(mediabost)
    
    
  }
  
  
  return(list(medias,desviacion))
  
}




bostCL(cla,10,20)









clases <- names(table(cla["clases"]))

clases


pos <- which(cla[,1]=="E")

cla[pos,2]



