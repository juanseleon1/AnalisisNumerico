#Por: Maria Garces && Juan Leon
n=as.double(readline("Ingrese un numero, cuya raiz desea encontrar:"))
x=as.double(readline("Ingrese un valor inicial:"))

f<-function(n,E,x)
{
  y<-0.5*(x+(n/x))
  repeat{
    x<-y
    y<-0.5*(x+(n/x))
    if(abs(x-y)>E)
      break
  }
 return(y) 
}

cat(format(toString(f(n,0.000001,x)),nsmall = 6))