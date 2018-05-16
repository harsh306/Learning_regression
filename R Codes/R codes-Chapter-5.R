# Codes for Cahpter-5

# Matrix Approach to Simple Linear Regression
#================================================================

# Getting Started
library(MASS)   #most of the matrix commands need this package.

#1. Defining a matrix,

A = c(4, 7, 10) # vector
A

AA=as.matrix(A) #Transform avector to a matrix
AA

B=matrix(c(1,2,3,4,5,6,7,8,9,10),ncol=2,nrow=5) #matrix with 2colms and 5 rows.
B

#2. Transpose of a matrix.
TransA=t(A)
TransA



TransB=t(B)
TransB

#3. Matrix Addition

C=matrix(c(rep(1,5),rep(2,5)),ncol=2,nrow=5) #matrix with 2colms and 5 rows.
C

sumBC=B+C
sumBC

#4. Matrix Subtraction
BminC=B-C
BminC

#5. Constant times a Matrix 

FTB=4*B
FTB

#6.Matrix Multiplication

BTtC=B%*%t(C) #dimentions shoud match
BTtC

#7. Inverse of a matrix.
library(MASS) #loading MASS
D=matrix(c(rep(1,3),rep(2,3),rep(3,3)),,ncol=3)
InvD=ginv(D)  #Should be a square matrix.
InvD



#Plastic hardness example.

X=cbind(rep(1,16),c(16,16,16,16,24,24,24,24,32,32,32,32,40,40,40,40)) #Creating the matrix X
Y=as.matrix(Hardness)

#1.
#To claculate the inverse of a matrix, MSSS package shoul used.
library(MASS) #loading MASS
A=ginv(t(X)%*%X) #here "t" is for the transpose and "ginv" is for the inverse
A               # %*% is for when we multify a matrix with another matrix.

#2
b=ginv(t(X)%*%X)%*%t(X)%*%Y
b

#3.
Yhat=X%*%b
Yhat

#4.
H=X%*%ginv(t(X)%*%X)%*%t(X)
H

#5.
SSE=t(Y)%*%Y-t(b)%*%t(X)%*%Y
SSE


#6.
Xh=c(1,30) 
Yhat_h=t(Xh)%*%b
Yhat_h



n=length(X)
MSE=SSE/(n-2)
MSE
S2b=as.vector(MSE)*ginv(t(X)%*%X) # Here note that you first need to convert MSE to a vector
S2b

#7.
Xh=c(1,30) 
S2pred=as.vector(MSE)*(1+t(Xh)%*%ginv(t(X)%*%X)%*%Xh)
S2pred
