##GaoSiqin_2017310895_L1.R
##Proof of higher algebraic theorem

###########################
#Generate the required matrix
mtrx1 <- matrix (1:16, 4, 4)
mtrx2 <- matrix (2:13 ,4, 4)
mtrx3 <- matrix ( rnorm(16) , 4, 4)
mtrx4 <- mtrx1%*%mtrx2
mtrx5 <- matrix(c(1,2,4,9),2,2)

#First, the determinant related verification

#1.Row and column swaps, determinant unchanged
Tmtrx1 <- t ( mtrx1 )
all.equal ( det (mtrx1),det (Tmtrx1) )

#2.A line of public factors can be extracted, equivalent to this male factor multiplied by this determinant
Mul.mtrx1 <- mtrx1
Mul.mtrx1[ , 1] <- Mul.mtrx1[ , 1]*2
all.equal(det (Mul.mtrx1) , 2*det (mtrx1))

#3.If a row is two sets of numbers and. Then the determinant is equal to two determinant and.

all.equal( det (Mul.mtrx1) ,det (mtrx1) + det (mtrx1))

#4.If two rows are equal in the determinant, the determinant is 0
trow.equal <- mtrx1
trow.equal [ , 1] <- trow.equal [ , 2]
all.equal(det ( trow.equal ), 0)

#5.Add a multiple of a row to another line, the determinant remains the same
Added <- mtrx1
Added [ , 1] <- Added[ , 1]+Added [ , 2]
all.equal(det ( Added ), det ( mtrx1 ))

#6.Swap two lines of determinant position, determinant inverse number
Swaped.mtrx <- mtrx1
Swaped.mtrx [ , 1] <- mtrx1 [ , 2] 
Swaped.mtrx [ , 2] <- mtrx1 [ , 1]
all.equal(det ( Swaped.mtrx ), det ( mtrx1 ))

#7.The sufficient requirement for a determinant of 0 is that the rank of the matrix is less than n
qr(mtrx1)$rank <= 4


#Second,validation related to matrix calculation
#8.Exchange Law
all ( Tmtrx1 + mtrx1 == mtrx1 + Tmtrx1)

#9.Binding Law
all (  Tmtrx1 + mtrx1 + Added ==Added + mtrx1 + Tmtrx1)

#10.The determinant of the product of a matrix is equal to the product of the determinant of its factor

all.equal ( det (mtrx1)*det (mtrx2) , det (mtrx1 %*% mtrx2))

#11.The rank of the product does not exceed the rank of each factor
Rmtrx1=qr(mtrx1)$rank
Rmtrx2=qr(mtrx2)$rank
Rmtrx4=qr(mtrx4)$rank
Rmin=min(Rmtrx1,Rmtrx2)
Rmtrx4 <= Rmin

#12.A-1 = 1/d*A*
library("nlme")
library("LoopAnalyst")
mtrx3.adjoint <- make.adjoint(mtrx3)
Dmtrx3 <- det(mtrx3)
Inv.mtrx3 <- solve ( mtrx3 )
all ( Inv.mtrx3 + mtrx3.adjoint/Dmtrx3  <= 1E-5)


#13.R£¨A£©=R£¨PA£©,P is reversible matrix
if (det (mtrx3) !=0) {
  qr(mtrx1)$rank == qr(mtrx1%*%mtrx3)$rank
  }


#14.Multiplication of block matrices
ma_zero <- matrix ( 0, 4, 4)
c1 <- cbind ( mtrx1, ma_zero )
c2 <- cbind ( ma_zero, mtrx3)
Fmtrx <- rbind ( c1 , c2)
det( Fmtrx )*det( mtrx1 ) - det (Fmtrx) <= 1E-5

#15. Multiplication of reversible matrix and original matrix as Unit matrix
if (det (mtrx3) !=0) {
  all (Inv.mtrx3 %*% mtrx3 - diag ( 4 ) <= 1E-5)
}

#16.Eigenvectors linear Independent

det ( eigen ( mtrx3 )$vectors) !=0

#17.Van der Mont Row style

det ( mtrx5 ) - (2-1) <= 1E-5

#18.If AB=0,r(A)+r(B)<=n
N18_1=matrix(c(1,0,0,0),2,2)
N18_2=matrix(c(0,0,0,1),2,2)
N18=N18_1%*%N18_2
det(N18)
qr(N18_1)$rank+qr(N18_2)$rank<=2

#19.The determinant of the inverse of the matrix is equal to the countdown of the determinant
det ( Inv.mtrx3 ) * det ( mtrx3 ) - 1 <= 1E-5

#20.R(AB)<=R(A)
qr( mtrx4 )$rank<=qr( mtrx1 )$rank

