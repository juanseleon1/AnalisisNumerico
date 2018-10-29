dy<-function(x,y)
{
  a<-(1-(x**2))+x+y
  return(a)
}
metodoEulerv1 <- function(f, h, xi, yi, xf)
{
  N = (xf - xi) / h
  x = y = numeric(N+1)
  x[1] = xi; 
  y[1] = yi;
  i = 1
  while (i <= N)
  {
    x[i+1] = x[i]+h
    y[i+1] = y[i]+((h/2)*f(x[i],y[i]))
    i = i+1
  }
  return (data.frame(X = x, Y = y))
}

metodoEuler <- function(f, h, xi, yi, xf)
{
  N = (xf - xi) / h
  x = y = numeric(N+1)
  x[1] = xi; 
  y[1] = yi;
  i = 1
  while (i <= N)
  {
    x[i+1] = x[i]+h
    y[i+1] = y[i]+(h*f(x[i],y[i]))
    i = i+1
  }
  return (data.frame(X = x, Y = y))
}
e1<-metodoEuler(dy, 0.1, 0, 1, 0.9)
e2<-metodoEulerv1(dy, 0.1, 0, 1, 0.9)
xe<-e1[,1]
aux<-e1[,2]
aux2<-e2[,2]
cat("x \tValor Euler \t\tValor Euler Variacion \t\tError\n")
for(i in c(0:length(aux)))
{
  cat(xe[i],"\t",aux[i],"\t\t",aux2[i],"\t\t",abs(aux[i]-aux2[i]),"\n")
}
