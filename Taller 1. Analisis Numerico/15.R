#Por: Maria Garces && Juan Leon
f<-function(x)
{
  return((5*x)-exp(x)-1)
}

plot(f,xlim = c(-2,3),col="Blue",lwd=2,type="l")
title("Raices de F(x)")
abline(h=0,lwd=1,col="Black")
x=uniroot(f,interval = c(0,0.6))
x1<-as.double(x[1])
points(x=x1,y = f(x1),col="Red",pch=19)
x=uniroot(f,interval = c(1,2.9))
x1<-as.double(x[1])
points(x=x1,y = f(x1),col="Red",pch=19)
  
g<-function(x){
  
  return((6*x)-exp(x)-1)
}
#Esta funcion fue tomada del libro
pfijo<-function(f,E,xo,maxi)
{
 d<-1
 k<-0
 xold<-xo
  repeat
  {
    xnew<-f(xold)
    d<-abs(xnew-xold)
    xold<-xnew
    k=k+1
    if(d<E || k>=maxi)
      break
    
  }
  cat("El valor de la raiz despues de ",k,"iteraciones, es de: ", xnew, ",esta posee un error de :",d,"\n")
}
#Utilizamos la funcion pfijo para intentar calcular la raiz. Esta corresponde a FixedPoint en metodo simple.

pfijo(g,0.001, 0,5)
#Metodo FixedPoint Simple
f1<-FixedPoint(g,Inputs =0,Outputs = c(),MaxIter = 5,Method = "Simple")
xnew<-as.double(f1[4])
cat("El valor de la raiz,con FixedPoint metodo simple, despues de 5 iteraciones, es de: ", xnew,"\n")
#La funcion FixedPoint se utiliza con el metodo de Aceleracion Anderson con el fin de obtener respuestas concretas
f1<-FixedPoint(g,Inputs =0,Outputs = c(),MaxIter = 5)
xnew<-as.double(f1[4])
cat("El valor de la raiz,con FixedPoint despues de 5 iteraciones, es de: ", xnew,"\n")