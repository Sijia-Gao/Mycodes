##GaoSiqin_2017310895_L1.R
##Proof of higher algebraic theorem

###########################
#Generate the required matrix
mtrx1 <- matrix (1:16, 4, 4)
mtrx2 <- matrix (2:13 ,4, 4)
mtrx3 <- matrix (seq (5 ,80 , 5))

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

#11.乘积的秩不超过各因子的秩
N11=N1%*%N2
rN11=qr(N11)$rank
rN1=qr(N1)$rank
rN2=qr(N2)$rank
minN1N2=min(rN1,rN2)
rN11<=minN1N2

#12.A-1（A的逆）=1/d*A*(伴随矩阵)
#因为要求伴随矩阵，所以用了LoopAnalyst的包
install.packages("LoopAnalyst")
install.packages("nlme")
library("nlme")
library("LoopAnalyst")
N12=matrix(rnorm(9),3,3)
N12_adjoint=make.adjoint(N12)
d=det(N12)
#####
#求N12的逆矩阵
N12_inv=solve(N12)
N12_inv-N12_adjoint/d<= 1E-5
#?all.equal(N12_inv,N12_adjoint/d)为什么报错

#13.R（A）=R（PA）,P为可逆矩阵
P=N12
det(P)==0
N13=matrix(1:9,3,3)
qr(N13)$rank==qr(N13%*%P)$rank

#14.分块矩阵的乘法
ma_zero=matrix(0,2,2)
m14=matrix(rnorm(4),2,2)
m15=matrix(rnorm(4),2,2)
N14_1=cbind(m14,ma_zero)
N14_2=cbind(ma_zero,m15)
N14=rbind(N14_1,N14_2)
det(m14)*det(m15)-det(N14)<=1E-5

#15. 可逆矩阵与原矩阵相乘为单位矩阵
N15=matrix(rnorm(9),3,3)
N15_inv=solve(N15)
N15%*%N15_inv

#16.特征向量线性无关
N16=matrix(1:9,3,3)
det(eigen(N16)$vectors)

#17.范德蒙行列式
N17=matrix(c(1,2,4,9),2,2)
det(N17)-(2-1)<=1E-5

#18.如果AB=0;那么r(A)+r(B)<=n
N18_1=matrix(c(1,0,0,0),2,2)
N18_2=matrix(c(0,0,0,1),2,2)
N18=N18_1%*%N18_2
det(N18)
qr(N18_1)$rank+qr(N18_2)$rank<=2

#19.矩阵逆的行列式等于行列式的倒数
N19=matrix(rnorm(4),2,2)
N19_inv=solve(N19)
det(N19)*det(N19_inv)-1<=1E-5

#20.R(AB)<=R(A)
N20_1=matrix(rnorm(9),3,3)
N20_2=matrix(rnorm(9),3,3)
qr(N20_1%*%N20_2)$rank<=qr(N20_1)$rank

