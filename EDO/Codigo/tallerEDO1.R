library(pracma)
dT<-function(t,Te)
{
  e<-(-1.68*10**-9)
  T0<-Te**4
  T4<-T0*t
  te<-200**4
  te<-e*te
  up<-(e*(T4))-te
  return(up)
  
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


e1 = metodoEuler(dT, 10, 0, 180, 200)
e1[nrow(e1),]

xx <- c(-10, 200); yy <- c(0, 200)
vectorfield(dT, xx, yy, scale = 0.1,xlab = "Tiempo",ylab = "T")
for (xs in seq(0, 200, by = 10.5)) 
{
  sol <- rk4(dT, 0, 200, xs, 100)
  lines(sol$x, sol$y, col="purple")
}

