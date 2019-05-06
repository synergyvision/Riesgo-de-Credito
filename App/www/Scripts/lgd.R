library(plotly)

lgd <- read.csv("~/Riesgo_de_Credito/App/appTemplate/data/loss_given.csv", sep=";")

nc <- dim(lgd)[2]





lgdp <- NULL




for (i in 1:nc) {
  
  lgdp[i] <- median(lgd[,i])
  
}

lgdp <- as.data.frame(lgdp)

lgdp[2] <- 1:24



colnames(lgdp) <- c("Porcentajes","Periodos")


p <- plot_ly(lgdp, x = ~Periodos, y = ~Porcentajes, name = 'trace 0', type = 'scatter', mode = 'lines') 
p
# library













