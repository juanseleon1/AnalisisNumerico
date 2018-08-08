#Por: Maria Garces && Juan Leon
Fx<-function(x){
  s<-(4*(x**3)-112*(x**2)+778*x)
  return(s)
}
nseccion<- function(eq,xl,xu,n,E) {
  f=eq(xl)*eq(xu)
  if (f<=0 ){
    d=(abs(xl-xu)/n)
    #**NUEVO**: Se calcula el numero de iteraciones
    itera=(log((d/E),n))
    cat("El valor de iteraciones teorico ",toString(round(itera,0)),"\n")
    #Se inicia la n-seccion
    cont=0
    while(d>E)
    {
      
      neg=FALSE
      #Se calculan los n+1 numeros que seran los nuevos limites a estudiar
      x<-0:n
      y<-c(0:n)
      y[0:n+1]<-xl
      x<-x*d
      x<-x+y
      i=2
      #Ciclo para revisar los nuevos intervalos buscando a aquel que se pueda usar para la n-seccion
      while(i<=n+1&&(neg==FALSE))
      {
        xl=x[i-1]
        xu=x[i]
        p=xl
        o=xu
        if(is.na(eq(p)))
        {p=p+0.00000001}
        if(is.na(eq(o)))
        {o=o+0.00000001}
        f=eq(p)*eq(o)
        if(f<=0)
        {
          neg=TRUE
        }
        i=i+1
      }
      #e calcula la distancia total entre los limites dados y se guarda en d
      d=(abs(xl-xu)/n)
      cont=cont+1
    }
    cat("# de Iteraciones: ",cont, "\n")
    #Se informa el valor aproximado
    if(eq(xl)==0)
    {
      cat("El valor aproximado es: ",toString(format(xl,nsmall = 8)),"\n")
    }else
    {
      cat("El valor aproximado es: ",toString(format(xu,nsmall = 8)),"\n")
    }
    
  }else
  {
    cat("Error: Xl y xu son del mismo signo.")
  }
  
}

df<-function(x)
{
 return(12*(x**2)-224*x+768)
}
ddf<-function(x)
{
  return(24*x-224)
}

newton<-function(f,df,E,xo)
{
  if((24-2*xo)<=0)
  {
   xo<-runif(1,0.1,11.99)
   cat("Debido a las restricciones del problema, 24-2x no puede ser menor a cero, su nuevo valor es: ", xo, "\n")
  }
  k=0
  repeat{
    c=f(xo)/df(xo)
    x1=xo-c
    dx=abs(c)
    xo=x1
    k=k+1
    if(dx<E)
      break;
  }
  cat("# de iteraciones: ",k," El valor de xmax: ",x1," f(x): ",f(x1), "Error Estimado: ", c)
}
cat("Para el metodo de n-seccion:")
xl=as.double(readline("Ingrese el valor de xl: "))
xu=as.double(readline("Ingrese el valor de xu: "))
n=as.double(readline("Ingrese el valor de particiones que desea: "))
nseccion(df,xl,xu,n,0.00000001)
cat("Para el metodo de newton:")
xo=as.double(readline("Ingrese el valor de xo: "))
newton(df,ddf,0.00000001,xo)