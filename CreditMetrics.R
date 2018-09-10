##Supongamos que exiten las siguientes calificaciones crediticias
##A,B,C,D,E


MT <- matrix(c(96.5,1.9,0.9,0.6,0.1,29.8,24.2,28,8,10,13.2,5.5,17.7,8.7,54.9,7.6,2.2,2.5,14,73.7,2.7,0.6,0.4,0.5,95.8),ncol = 5, nrow = 5,byrow = T)
colnames(MT) <- c("A","B","C","D","E")
rownames(MT) <- c("A","B","C","D","E")

apply(MT,1,sum)

##Tasas de interes
## En general las tasas de interes varian con el tiempo
## Como estamos en una cartera de micro creditos 
## en general se usa ua tasa fija para todos  (24%)
## pero puede variar por cartera 
## e incluso ponerse a variar por calificacion

## dependiendo de cada clase se espera recuperar del credito cierta cantidad


RP <- c(0.999,0.95,0.87,0.75,0.55)


###En realidad necesitamos son el valor que cobrara el banco de un credito por ejemplo
###por ejemplo si damos un credito de 100 unidades, al final cobraremos es 124


creditos <- c(7000,8000,4440,3270,1900,4720,2000,5500,3000,4500,3700,880,4400,7000,3015,2490,2200,1390,6730,1670,3579,1340,1500,1350,2000,500,250,1720,2350,1600,1400,500,300)

names(creditos) <- c("A","A","A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B","B","B","B","B","B","C","C","C","D","D","D","E","E","E") 



sum(creditos[which(names(creditos)=="A")]*(1-RP[1]))+ sum(creditos[which(names(creditos)=="B")]*(1-RP[2]))+ sum(creditos[which(names(creditos)=="C")]*(1-RP[3]))+ sum(creditos[which(names(creditos)=="D")]*(1-RP[4]))+ sum(creditos[which(names(creditos)=="E")]*(1-RP[5]))
sum(creditos)

464711/96194


