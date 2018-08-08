#Por: Maria Garces && Juan Leon
#No olivde incluir las librerias pracma y FixedPoint

f<-function(x)
{
  return((5*x)-exp(x)-1)
}

n<-newtonRaphson (f, 0.5, dfun = NULL,maxiter = 100)

b<-bisect (f, 0, 1, maxiter = 100, tol = NA)


print("Newton Rhapson: ")
print(n)

print("Biseccion: ")
print(b)

f1<-function(x)
{
  return((5*x)-exp(x)-1)
}
f2<-FixedPoint(f1,Inputs =0,Outputs = c(),MaxIter = 5)
print("Punto Fijo: ")
print(f2)
