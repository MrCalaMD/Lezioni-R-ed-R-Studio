library(epiR)

library(foreign)

################################################################################

vettore <- c(1:100)

matrice <- matrix(vettore, 5, 20)

x <- seq(0, 1, 0.01)

y <- sin(pi*2 * x)

plot(x, y)  

######################## Colname e Rowname #####################################

rownames(matrice) <- LETTERS[1:5]
colnames(matrice) <- letters[1:20]

matrice

################################################################################

