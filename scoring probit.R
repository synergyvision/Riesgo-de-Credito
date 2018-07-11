##  scoring modelo probit


data <- read.csv("C:/Users/arturo.carreno/Downloads/default-of-credit-card-clients-dataset/UCI_Credit_Card.csv")

probit <- glm(data$default.payment.next.month ~ data$LIMIT_BAL + data$SEX + data$EDUCATION + data$MARRIAGE, family = binomial(link = "probit"))
summary(probit)
