histo <- read.csv("~/Riesgo-de-Credito/Historica_de_perdidas.csv", sep=";", header = T)

N <- NULL


clases <- levels(histo[,1])

for (i in 1:length(clases)) {
  
  s <- which(histo[,1]==clases[i])
  
  s1 <- histo[,2]
  
  N[i] <- mean(s1[s])
  
}

result <- data.frame(clases,N)

colnames(result) <- c("Perdida","Calif")

result










