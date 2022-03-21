sqrt(3^2+4^2)
1- pchisq(4.3,1)
w<-c(10, 13, 9, 1, -1, 2, -2, -4, -8)
w
which(w<1 | w>4)
w[which(w<1 | w>4)]
str(w)
v<-w+1
v
y<-c(0,1,5,10,seq(15,75,10))
y
x<-c(1,3,5)
y<-c(-3,2,4)
z<-c(10,13,-9)
A<-matrix(cbind(x,y,z),3,3)
str(A)
A
rownames(A)<-c("a","b","c")
A
is.matrix(A)
subA<-A[1:2,1:2]
subA
B<-3*A
B
which(B>5 & B<15)