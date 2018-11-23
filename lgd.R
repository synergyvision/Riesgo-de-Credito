lgd <- read.csv("~/Riesgo_de_Credito/App/appTemplate/data/loss_given.csv", sep=";")

nc <- dim(lgd)[2]





lgdp <- NULL




for (i in 1:nc) {
  
  lgdp[i] <- median(lgd[,i])
  
}

lgdp <- as.data.frame(lgdp)
colnames(lgdp) <- c("Porcentajes")


plot(100-lgdp)




# library
library(ggplot2)

# Iris dataset


# plot

qplot(seq_along(lgdp), lgdp)












