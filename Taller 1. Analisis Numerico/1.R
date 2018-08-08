#Por: Maria Garces && Juan Leon
horner<- function(p,n,x) {
  y=p[1]
  m=c(p[1])
  for (i in c(2:n)) {
      y=x*y+p[i]
      m[i]=y
  }
  cat("El numero de operaciones fue: ", 2*n,"\n")
  return(m)
}

eval<-function(p,n,x){
  s=0
  for (i in c(1:n)){
    s=s+p[i]*(x**(n-i))
  }
  return(s)
}
n=as.integer(readline("Ingrese el tamanio del polinomio: "))
x=as.integer(readline("Ingrese el valor para evaluar el polinomio: "))
p=readline("Ingrese los coeficientes del polinomio, separados por , : ")
poli=unlist(strsplit(gsub("(c\\(|\\))","",p),","))
pol=as.numeric(poli)
aux1<-horner(pol,n,x)
cat("El resultado de Horner:",aux1,"\n")
aux<-eval(pol,n,x)
cat("Al evaluar el polinomio en ",x, ": ",aux,"\n")
resultado<-eval(aux1,n-1,-2)
cat("El valor de la derivada del polinomio en ", x,": ", resultado," , el numero de operaciones fue:",2*(n-1),"\n")