
fx<-expression(sin(x))
g <- D(fx, "x")
r<-as.function(alist(x=, eval(g)))

ecuacion<-function(x)
{
  res<-sqrt(1+(r(x)**2))
  return(res)
}

res<-integrate(ecuacion, 0, 2)
print(res)
