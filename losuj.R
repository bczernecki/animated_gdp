lista = read.csv("data/studenci.csv")

lista$numer = sample(0:7,  replace = F)
lista$numer[lista$numer==7]=0
