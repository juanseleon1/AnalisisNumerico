library(Ryacas)
library(pracma)

f <- expression(1-x^2+x+y)
fe <- expression(exp(x)+x^2+x)
fr <- function(x,y){1-x^2+x+y}

metodoTaylor <- function(f, h, xi, yi, xf)
{
  N = (xf - xi) / h
  funcion=f
  xr = yr = numeric(N+1)
  z = numeric(N+1)
  xr[1] = xi; 
  yr[1] = yi;
  i = 1
  final = 0
  ry = 0
  
  while(i<=N)
  {
    ry=0
    
    xr[i+1] = xr[i]+h
    final=yi
    x<-xi; y<-yi
    fx=f
    
    for(n in 1:3)
    {
      ry=ry+eval(fx)
      e<-(ry)*((xr[i+1]^n)/factorial(n))
      final=final+e
      funcion=fx
      fx<-D(funcion,'x')
    }
    yr[i+1] = final
    i=i+1
  }
  
  for(j in 1:5)
  {
    x<-xr[j]
    z[j]=(abs(eval(fe)-yr[j])/eval(fe))*100
  }
  
  return (data.frame(X = xr, Y = yr, Error = z))
}

r=metodoTaylor(f, 0.1, 0, 1, 0.4)
print(r)

xx <- c(-1, 2); yy <- c(0, 3)
vectorfield(fr, xx, yy, scale = 0.1)
for (xs in seq(0, 0.4, by = 0.1)) 
{
  sol <- rk4(fr, 0, 1, xs, 100)
  lines(sol$x, sol$y, col="purple")
}

