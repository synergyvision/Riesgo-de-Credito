library(Rdice)

##Supongamos que exiten las siguientes calificaciones crediticias
##A,B,C,D,E


MT <- matrix(c(96.5,1.9,0.9,0.6,0.1,29.8,24.2,28,8,10,16,7.8,14.9,10.3,51,7.6,2.2,2.5,14,73.7,2.7,0.6,0.4,0.5,95.8),ncol = 5, nrow = 5,byrow = T)
colnames(MT) <- c("A","B","C","D","E")
rownames(MT) <- c("A","B","C","D","E")
clasi <- c("A","B","C","D","E")
apply(MT,1,sum)
write.table(MT, "Matiz.txt", sep = "\t", quote = F, row.names = T)
##Tasas de interes
## En general las tasas de interes varian con el tiempo
## Como estamos en una cartera de micro creditos 
## en general se usa ua tasa fija para todos  (24%)
## pero puede variar por cartera 
## e incluso ponerse a variar por calificacion

## dependiendo de cada clase se espera recuperar del credito cierta cantidad


RP <- c(0.001,0.05,0.13,0.25,0.45)


###En realidad necesitamos son el valor que cobrara el banco de un credito por ejemplo
###por ejemplo si damos un credito de 100 unidades, al final cobraremos es 124


creditos <- c(7000,8000,4440,3270,1900,4720,2000,5500,3000,4500,3700,880,4400,7000,3015,2490,2200,1390,6730,1670,3579,1340,1500,1350,2000,500,250,1720,2350,1600,1400,500,300,7000,8000,4440,3270,1900,4720,2000,5500,3000,4500,3700,880,4400,7000,3015,2490,2200,1390,6730,1670,3579,1340,1500,1350,2000,500,250,1720,2350,1600,1400,500,300,7000,8000,4440,3270,1900,4720,2000,5500,3000,4500,3700,880,4400,7000,3015,2490,2200,1390,6730,1670,3579,1340,1500,1350,2000,500,250,1720,2350,1600,1400,500,300,7000,8000,4440,3270,1900,4720,2000,5500,3000,4500,3700,880,4400,7000,3015,2490,2200,1390,6730,1670,3579,1340,1500,1350,2000,500,250,1720,2350,1600,1400,500,300)

sum(creditos*0.36)



names(creditos) <- c("A","A","A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B","B","B","B","B","B","C","C","C","D","D","D","E","E","E","A","A","A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B","B","B","B","B","B","C","C","C","D","D","D","E","E","E","A","A","A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B","B","B","B","B","B","C","C","C","D","D","D","E","E","E","A","A","A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B","B","B","B","B","B","C","C","C","D","D","D","E","E","E") 



sum(creditos[which(names(creditos)=="A")]*(1-RP[1]))+ sum(creditos[which(names(creditos)=="B")]*(1-RP[2]))+ sum(creditos[which(names(creditos)=="C")]*(1-RP[3]))+ sum(creditos[which(names(creditos)=="D")]*(1-RP[4]))+ sum(creditos[which(names(creditos)=="E")]*(1-RP[5]))
sum(creditos)

t <- proc.time()
M <- NULL
for (j in 1:10000) {
  

N <- NULL 
for (i in 1:length(clasi)) {
  l <- creditos[which(names(creditos)==clasi[i])]
  g <- dice.roll(faces=length(clasi), dice=length(l), rolls=1, weights=as.numeric(MT[i,]/100))
  
  N[i] <- sum(l*RP[as.numeric(g$results[,1:length(l)])])
}

M[j] <- sum(N)

}
mean(M)
sd(M)
proc.time()-t    # Detiene el cronómetro
hist(M)

var <- qnorm(0.95,mean = mean(M),sd = sd(M))

sum(creditos)








l <- creditos[which(names(creditos)==clasi[2])]
l
g <- dice.roll(faces=length(clasi), dice=length(l), rolls=1, weights=as.numeric(MT[2,]/100))
g$results[,1:length(l)]
sum(l * RP[as.numeric(g$results[,1:length(l)])])
sum(l)

mean(M)
sd(M)


l <- creditos[which(names(creditos)==clasi[3])]

dice.roll(faces=5, dice=3 , rolls=1, weights=as.vector(MT[3,]/100))
    
varnormal(.95, mean(M), sigma=sd(M), log.p=FALSE, lower.tail=TRUE)

qnorm(.95)*sd(M)+mean(M)








