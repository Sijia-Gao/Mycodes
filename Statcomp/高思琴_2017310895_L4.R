##GaoSiqin_2017310895  L4
##In this file,I will complete three tasks

######## Task 1: use while to judge  leap year ######
Years <- seq ( 1000 , 2020 , 1)
i <- 1
Leapyear=vector
NotLeapyear=vector
while (i <= length (Years)){
  year <- Years[i]
  i <- i+1
  if ( year%%4 == 0 & year%%100 !=0){
    Leapyear=cbind(Leapyear,year)
    
  }else if ( year%%400 == 0){
    Leapyear=cbind(Leapyear,year)
  }else{
    NotLeapyear=cbind(NotLeapyear,year)
  }
}
  
Leapyear
NotLeapyear


####### Task 2 :Compare the time ##############

####### (1) the time of function ####
Start.fun <- Sys.time()
is.Leap <- function(Years){
  whthr <- Years[(Years%%4 == 0 & Years%%100 !=0) | +
                (Years%%400 == 0)]
  return(whthr)
}
replicate ( n <- 100000 , is.Leap(Years))
End.fun <- Sys.time()
End.fun - Start.fun

######## (2) the time of circulation ###########
Start.cir <- Sys.time()
Leap <- vector
NotLeap <- vector
j <- 1
# j is used to control the times of circulation
while(j <= 100){
for (year in 1000:2020){
  if ( year%%4 == 0 & year%%100 !=0){
    Leap=cbind(Leap,year)
  }else if ( year%%400 == 0){
    Leap=cbind(Leap,year)
  }else{
    NotLeap=cbind(NotLeap,year)
  }
}
  j <- j+1
}

End.cir <- Sys.time()
End.cir - Start.cir

#### $_$ my computer almost break down when j<=10000!!!0###

######### Task 3: Create my own function #####

#============= COPY %*% ===============#
Mul.mtrx <- function( mtrx1 , mtrx2 ){
#++++++ Gurantee the parameter are matrixs ++++++#
  if ( class ( mtrx1 ) != "matrix" & 
       class ( mtrx2 ) != "matrix"){
    print ("Class Error!!! Please input two matrixs.")
       }else if ( ncol( mtrx1 ) != nrow ( mtrx2 ) ){
         print ("Dimension Error!!!")
       }else{ 
         r1 <- nrow ( mtrx1)
         c2 <- ncol ( mtrx2)
         s <- matrix ( 0 , r1 ,c2)
         for ( i in 1:r1)
           for ( j in 1:c2)
             s[i,j] <- sum(mtrx1[i,]*mtrx2[,j])
         return(s)
    
  }
}
a=matrix(1:6,2,3)
b=matrix(1:6,3,2)
Mul.mtrx(a,b)

#============ Copy:solve ===================#
LU.mtrx <- function(A){
  n <- dim(A)[1]
  L <- diag(1,n,n)
  U <- matrix(0,n,n)
  U[1,]=A[1,]
  L[,1]=A[,1]/U[1,1]
  for (r in 1:(n-1)){
    flag=1
    for (c in r:(n-1)){
          U[r+1,c+1]=A[r+1,c+1]-L[r+1,]%*%U[,c+1]
          if ((r+1<n)&(flag==1)){
          for (k in 1:r+1){
                L[r+2,k] = (A[r+2,k]-L[r+2,]%*%U[,k])/U[k,k]
          }
        
          flag=flag+1
       
         }
      }
  
  }
  output=list(L,U)
  names(output)=c("L","U")
  return(output)
}

Solve.L=function(L){
  n <- dim(L)[1]
  Inv.L <- diag(1,n,n)
  for (i in 1:n){
    if (i+1<n){
      for (k in (i+1):n){
        for (j in i:(k-1)){
          Inv.L[k,i]=Inv.L[k,i]-L[k,j]*Inv.L[j,i]
      }}
    
   }
    
  }
  return(Inv.L)
}

Solve.U=function(U){
  n <- dim(U)[1]
  dia.U=diag(U)
  Inv.U=diag(1/dia.U,n,n)
  for (i in 1:n){
    if ( (i-1) >= 1){
      for (j in (i-1):1){
        s=0
        for ( k in (j+1):i){
          s=s+U[j,k]*Inv.U[k,i]
          
        }
        Inv.U[j,i]=-s/U[j,j]
      }
    }
  }
  return(Inv.U)
}

copy.solve = function(mtrx){
  LU.list=LU.mtrx(mtrx)
  L=LU.list$L
  U=LU.list$U
  Inv.mtrx=Solve.U(U)%*%Solve.L(L)
  return(Inv.mtrx)
}  

B=matrix(rnorm(25),5,5)
copy.solve(B)
solve(B)

