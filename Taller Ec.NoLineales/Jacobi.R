jacobi<-function(A1,B,xo,E)
{
  
  cont<-1
  fin<-1
 repeat
  {

    U<-A1
    L<-A1
    U[lower.tri(U,diag = TRUE)]<-0
    L[upper.tri(U,diag = TRUE)]<-0
    A<-A1-L-U
    D=solve(A)
    Z<-(D%*%(L+U))
    Tu<-Z*-1
    C<-D%*%B
    xo<-(Tu%*%xo)+C
    cont=cont+1
    fin<-(A%*%xo)-B
    if(final(A1,B,xo,E))
        {
          break
         }
  }
  cat("Xo:")
  print(xo)
  cat("Contador: ", cont)
  cat("\nEvaluando: ")
  print(A1%*%xo)
}

radio<-function(A1)
{
  u<-eigen(A1)
  u<-abs(as.double(unlist(u["values"])))
  u<-max(u)
  return (u)
}

final<-function(A,B,x,E)
{
   r<-A%*%x-B
  for(i in 1:nrow(r))
  {
    w<-r[i,1]
    if(abs(w)<E)
      return (TRUE)
  }
  return (FALSE)
}

A<-matrix(nrow = 3,ncol = 3,c(10,2,-1,-3,-6,2,1,1,5),byrow = TRUE)
B<-matrix(nrow = 3,ncol = 1,c(27,-61.5,-21.5),byrow = TRUE)
x<-matrix(nrow = 3,ncol = 1,c(-0.1,12,-6),byrow = TRUE)
jacobi(A,B,x,0.00001)