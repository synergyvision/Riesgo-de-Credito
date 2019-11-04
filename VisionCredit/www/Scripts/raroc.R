

library(pdftools)


text <- pdf_text("~/Desktop/banca.pdf")

text2 <- strsplit(text, "\n")

pag <- text2[[7]]

PagC <- pag[9]

vector <- strsplit(PagC, split = " ")
s <- vector[[1]]
cap <- s[54]
cap













  







