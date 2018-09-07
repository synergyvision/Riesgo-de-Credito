##Supongamos que exiten las siguientes calificaciones crediticias
##A,B,C,D,E


MT <- matrix(c(96.4,1.9,0.9,0.6,0.1,29.8,24.2,11,8,27.1,13.2,5.5,17.7,8.7,54.9,7.6,2.2,2.5,14,73.7,2.7,0.6,0.4,0.5,95.8),ncol = 5, nrow = 5,byrow = T)


##Tasas de interes
## En general las tasas de interes varian con el tiempo
## Como estamos en una cartera de micro creditos 
## en general se usa ua tasa fija para todos  (24%)
## pero puede variar por cartera 
## e incluso ponerse a variar por calificacion


TI <- 24

###En realidad necesitamos son el valor que cobrara el banco de un credito por ejemplo
###por ejemplo si damos un credito de 100 unidades, al final cobraremos es 124


Creditos <- c(3000,1500,700,400,1500,350,2000,5000,250,1720,2350,1600,3400,4100,3300)

names(Creditos) <- c("A","A","A","A","A","B","B","B","B","B","C","C","C","C","C") 
length(Creditos)
Creditos
º











