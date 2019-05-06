migra <- read.csv("~/Riesgo-de-Credito/Historica de migraciones.csv", sep=";")
clases <- levels(migra[,1])


periodos <- length(migra)/2

n <- NULL



for (k in 1:periodos) {
  
 

  for (j in 1:length(clases)) {
    
  
        orig <- length(which(migra[,(2*k)-1]==clases[j]))
        s <- migra[,2*k]
        s1<- s[which(migra[,(2*k)-1]==clases[j])]
        
        
        for (i in 1:length(clases)) {
        
          
              final <-length(which(s1==clases[i]))
                
                
              n[i+(5*(j-1))+(25*(k-1))] <- 100*final/orig
                
                
        }
    
      
  }

  

}





o <- matrix(numeric(25),nrow = 5)



for (b in 0:9) {
  
  h <- matrix(n[(1+(25*b)):(25+(25*b))],5, byrow = T)
  o <- o + h
  
  }

Matrix <- o / 10

MT <- round(Matrix)







