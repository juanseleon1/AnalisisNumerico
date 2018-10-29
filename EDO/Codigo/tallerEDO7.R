list.of.packages <- c("phaseR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(phaseR)

f<-function(fcn,x,y){
  return(eval(fcn))
}
dy<-function(x,y)
{
  a<-(1-(x**2))+x+y
  return(a)
}
fy<-function(x)
{
  return((x**2)+x+exp(x))
}
# Solo para prueba con dy=x+y, y(0)=1
obtenerErrorAbsoluto<-function(x,y){
  return(abs(0))
}

graficarCampoPendiente<-function(x0, xn, y0, yn, fcn, numpendientes, metodo){
  apma1 <- function(t, y, parameters){
    a <- parameters[1] 
    dy <- a*(f(fcn, t, y))
    list(dy)
  } 
  apma1.flowField <- flowField(apma1, x = c(x0, xn), 
                               y   = c(y0, yn), parameters = c(1), 
                               points = numpendientes, system = "one.dim", 
                               add = FALSE, xlab = "x", ylab = "y", 
                               main = metodo)
  grid()
}

graficarSolucionNumerica<-function (x, y){
  points (x, y, pch=20, col="blue")
  for (i in 2:length(x)){
    segments(x[i-1], y[i-1], x[i], y[i], col="red")
  }
}

rk4<-function(dy, ti, tf, y0, h, graficar=TRUE, numpendientes=10){
  t<-seq(ti, tf, h)
  y<-c(y0)
  cat("Rk4\n")
  cat("x    |y         |k1        |k2        |k3        |k4\n")
  for(i in 2:length(t)){
    k1=h*f(dy, t[i-1], y[i-1])
    k2=h*f(dy, t[i-1]+h/2, y[i-1]+k1*(0.5))
    k3=h*f(dy, t[i-1]+h/2, y[i-1]+k2*(0.5))
    k4=h*f(dy, t[i-1]+h, y[i-1]+k3)
    y<-c(y, y[i-1]+1/6*(k1+2*k2+2*k3+k4))
    cat(t[i-1]," | ", y[i-1]," | ",k1," | ",k2," | ",k3," | ",k4," |\n")
  }
  if (graficar){
    graficarCampoPendiente(min(t), max(t), min(y), max(y), dy, numpendientes, "RK4")
    graficarSolucionNumerica(t, y)
  }
  rta<-data.frame(X = t, Y = y)
}

rk3<-function(dy, ti, tf, y0, h, graficar=TRUE, numpendientes=10){
  t<-seq(ti, tf, h)
  y<-c(y0)
  cat("Rk3\n")
  cat("x    |y         |k1         |k2        |k3\n")
  for(i in 2:length(t)){
    k1=h*f(dy, t[i-1], y[i-1])
    k2=h*f(dy, t[i-1]+h/2, y[i-1]+k1*(0.5))
    k3=h*f(dy, t[i-1]+h, y[i-1]-k1+2*k2)
    y<-c(y, y[i-1]+1/6*(k1+4*k2+k3))
    cat(t[i-1]," | ", y[i-1]," | ",k1," | ",k2," | ",k3,"\n")
  }
  if (graficar){
    graficarCampoPendiente(min(t), max(t), min(y), max(y), dy, numpendientes, "RK3")
    graficarSolucionNumerica(t, y)
  }
  rta<-data.frame(X = t, Y = y)
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
e1<-metodoEuler(dy, 0.1, 0, 1, 0.9)
r4<-rk4(expression((1-(x**2))+x+y), 0, 0.9, 1, 0.1)
r3<-rk3(expression((1-(x**2))+x+y), 0, 0.9, 1, 0.1)
xe<-r3[,1]
aux<-r3[,2]
aux2<-r4[,2]
aux3<-e1[,2]
cat("x \tValor RK3 \t\tValor RK4 \t\tEuler \n")
for(i in c(0:length(aux)))
{
  cat(xe[i],"\t",aux[i],"\t\t",aux2[i],"\t\t",aux3[i],"\n")
}
curve(fy,add = T,col="Purple")
