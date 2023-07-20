A = matrix(c(2, 4, 3, 1, 5, 7), nrow=2, ncol=3, byrow = TRUE) 
A
B = matrix(c(1, 5, 3, 4, 2, 6), nrow=2, ncol=3, byrow = TRUE)
B
(C=A+B)
(D=t(A)%*%B)

(E = matrix(c(2, 1, 1,2), nrow=2, ncol=2, byrow = TRUE))
(F=solve(E))
(G=E%*%F)
(H=det(D))
 
