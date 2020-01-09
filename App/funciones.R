grafica <- function(datos,nom,nom2){
  
  s1 <- datos
  nombres <- colnames(datos)
  
  nombre <- nom
  
  posi <- which(nombres == nombre)
  
  nombre1 <- nom2
  
  posi1 <- which(nombres == nombre1)
  
  s1[,posi]<-  as.factor(s1[,posi])
  
  
  
  
  p10 <- ggplot(s1, aes(x = s1[[nombre]], y = s1[[nombre1]])) +
    geom_boxplot(fill = "#56B4E9") +
    scale_y_continuous(name = "Escala de valores") +  scale_x_discrete(name = "Categorias") +
    ggtitle("Comparación entre las categorias de la variable seleccionada") 
  return(p10)
  
  
  
}


estadf<-function(datos,nomb){
  
  s1 <- datos
  
  
  
  s2 <- rbind(summary(s1[[nomb]]))
  colnames(s2) <- c("Mínimo","Primer Quartil", "Mediana","Media", "Tercer Quartil", "Máximo")
  
  return(s2)
  
}





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
  rec <- NULL
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
    rec[i] <- round(qchisq(1-as.numeric(input$significancia),pr$parameter),4)
  }
  
  
  pval <- t(pval)
  est <- t(est)
  rec[i]
  
  vd <- nomb[which(pval > 0.05)]
  
  
  j <- colnames(datos)
  
  
  
  inf <- rbind(pval,est,rec)
  inf[1,1] <- "P-valor"
  inf[2,1] <- "Estadistico"
  inf[3,1] <- "Valor Crítico"
  
  colnames(inf) <- nomb
  colnames(inf)[1] <- ""
  return(inf)
}


### Se calculan los p-valores de las variables cuantitativas con la funcion pval.

pval. <- function(datos){
  
  
  D<- datos
  M <- c()
  
  
  for (i in 2:length(names(datos))) {
    
    if(length(summary(as.factor(D[[i]])))<=10){
      M[length(M)+1] <- i
      
    }
    
  }
  
  D1 <- as.data.frame(D[,-M])
  pval <- NULL
  est <- NULL
  rec <- NULL
  
  nomb <- colnames(D1)
  
  
  for (i in 2:length(nomb)) {
    
    df1 <- D1[,c(1,i)]
    
    d0 <- subset(df1, Creditability==0)
    d1 <- subset(df1, Creditability==1)
    
    p1 <- d0[[2]]
    p2 <- d1[[2]]
    
    
    w <-  ks.test(p1,p2)
    
    pval[i] <- round(w$p.value,4)
    est[i] <- round(w$statistic,4)
    
    
  }
  
  pval <- t(pval)
  est <- t(est)
  rec <- pval
  
  
  inf <- rbind(pval,est,rec)
  colnames(inf)<-nomb
  inf[1,1] <- "P-valor"
  inf[2,1] <- "Estadistico"
  inf[3,1] <- "Valor Crítico"
  colnames(inf)[1] <- ""
  return(inf)
  
  
}


coefglm <- function(modelo){
  
  l1 <- modelo$coefficients
  
  l2 <-names(modelo$coefficients)
  
  res <- t(rbind(l2,l1))
  
  res[1,1] <- "Punto de corte con el eje Y"
  colnames(res) <- c("Variables","Coeficientes")
  
  return(res)
  
}



estglm <- function(modelo){
  
  aic <- round(modelo$aic,2)
  
  ND <- round(modelo$null.deviance,2)
  
  RD <- round(modelo$deviance,2)
  
  fis <- modelo$iter
  
  Est <- c(aic,RD,ND,round(fis,1))
  
  inf <- data.frame(c("Criterio de información de Akaike","Desviación de los residuos","Desviación Nula","Número de iteraciones de Fischer"),Est)
  colnames(inf) <- c("Estadísticos","Resultado")
  
  return(inf)
  
}

