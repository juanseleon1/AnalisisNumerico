jacobi<-function(A1,B,xo,E)
{
  if(radio(A1)==1)
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
  }else
  {
    print("El metodo no converge pues el radio espectral es menor a 1")
  }
  
}

radio<-function(A1)
{

  U<-A1
  L<-A1
  U[lower.tri(U,diag = TRUE)]<-0
  L[upper.tri(U,diag = TRUE)]<-0
  A<-A1-L-U
 
  D=solve(A)
  A<-D**(L+U)
  u<-eigen(A)
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

A<-matrix(nrow = 3,ncol = 3,c(8,9,2,2,7,2,2,8,6),byrow = TRUE)
B<-matrix(nrow = 3,ncol = 1,c(69,47,68),byrow = TRUE)
x<-matrix(nrow = 3,ncol = 1,c(0,0,0),byrow = TRUE)
jacobi(A,B,x,0.0001)