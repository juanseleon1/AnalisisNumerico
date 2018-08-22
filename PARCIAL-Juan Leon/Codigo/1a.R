nExp<-function(n)
{
n1<-n*n
#El contador se empieza en 1 pues la primera operacion fue multiplicar n por n
cont<-1
suma<-0
for(i in 1:n1)
{
  #En cada iteracion se hace una suma y una multiplicacion
  suma=suma+(i*i)
  cont=cont+2
}
cat("La suma para ",n, " fue de: ", suma, " el numero de operaciones fue de: ",cont)
}
n=as.integer(readline(prompt="Ingrese n: "))
nExp(n)