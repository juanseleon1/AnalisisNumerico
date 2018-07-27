#Se expresa la funcion y se guarda en eq
eq<-function(x){-40+((((9.8*68.1)/x))*(1-exp(-10*(x/68.1))))}
#Se encuentra la raiz de eq
orig=uniroot(eq,lower=-1,upper = 20)
resp=orig["root"]
#Se imprime el valor obtenido por la maquina
cat("El valor de la raiz calculado por la maquina: ",toString(format(resp,nsmall = 8)))
#Obtencion de datos por consola
n=as.integer(readline(prompt="Ingrese el numero de particiones, mayor a 1: "))
E=as.double(readline(prompt="Ingrese la precision: "))
xl=as.double(readline(prompt="Ingrese valor xl: "))
xu=as.double(readline(prompt="Ingrese valor xu: "))
#Se revisa si es na en caso de que la funcion no este definida en ese valor
p=xl
o=xu
#Se suma 0.0000001 para que el programa continue la ejecucion en caso de que el valor xl o xu no esten definidos en la funcion
if(is.na(eq(p)))
{p=p+0.00000001}
if(is.na(eq(o)))
{o=o+0.00000001}
f=eq(p)*eq(o)
#Se verifica si los valores ingresados pueden ser utilizados para la n-seccion
if (f<=0 ){
  #Se abre una ventana de dispositivo grafico
  X11()
  #Se grafica eq y el punto de la raiz
  plot(eq,xlim=c(xl,xu), main="Funcion del paracaidista",xlab="Coeficiente", ylab="f(c)",col="blue")
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  points(resp,eq(as.numeric(resp)),col="red")
  abline(h=0)
  abline(v=0)
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
    cat("El valor aproximado es: ",toString(format(xl,nsmall = 8)))
  }else
  {
    cat("El valor aproximado es: ",toString(format(xu,nsmall = 8)))
  }
  
}else
{
  cat("Error: Xl y xu son del mismo signo.")
}
