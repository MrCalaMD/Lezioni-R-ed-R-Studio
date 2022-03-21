#2 Esercizio
sqrt(3**2 + 4**2)

#3 Esercizio
1-pchisq(4.3, df = 1)

#4 Esercizio
w <- c(10, 13, 9, 1, -1, 2, -2, -4, -8)

#5 Esercizio
which((w<1) | (w>4))
w[which((w<1) | (w>4))]

#6 Esercizio
str(w)

#7 Esercizio
v <- w+1
v

#8 Esercizio

y <- c(c(0, 1), c(seq(5, 75, 5)))

#9 Esercizio

x <- c(1, 2, 9)
y <- c(1, 2, 3)
z <- c(1, 2, 9)

A <- cbind(x, y, z)

rownames(A) <- c("a", "b", "c")

A

#10 Esercizio

is.matrix(A)

#11 Esercizio

subA <- A[1:2, 1:2]

subA

#12 Esercizio

B <- 3*A

#13 Esercizio

which((B>5) & (B<15))

B[which((B>5) & (B<15))]


