
#Elaborado por: Laura Donado y Jhonny Parra
#2018

list.of.packages <- c("pracma","deSolve")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(pracma)
library(deSolve)


solucionODE<-function(dy, t, h, y0){
  params <- list(a=1)
  fn <- function(x, y, params) with(params, list(a*dy(x, y)))
  out <- ode(y0, t, fn, params)
  return (out)
}

graficarCampoPendiente<-function(x0, xn, y0, yn, func, numpendientes, metodo){
  vectorfield (func, c(x0, xn), c(y0, yn), n=numpendientes, scale = 0.1, col="gray")
  mtext(side=3, metodo,font=2,cex=2)
}

graficarSolucionNumerica<-function (x, y){
  points (x, y, pch=20, col="blue")
  for (i in 2:length(x)){
    segments(x[i-1], y[i-1], x[i], y[i], col="red")
  }
}

rungekutta4<-function(dy, ti, tf, y0, h, graficar=TRUE, numpendientes=10){
  t<-seq(ti, tf, h)
  y<-c(y0)
  error<-c(0)
  solucion=solucionODE (dy, t, h, y0)
  #cat("x    |y         |k1        |k2        |k3        |k4       |error absoluto\n")
  
  for(i in 2:length(t)){
    k1=h*dy(t[i-1], y[i-1])
    k2=h*dy(t[i-1]+h/2, y[i-1]+k1*(0.5))
    k3=h*dy( t[i-1]+h/2, y[i-1]+k2*(0.5))
    k4=h*dy( t[i-1]+h, y[i-1]+k3)
    y<-c(y, y[i-1]+1/6*(k1+2*k2+2*k3+k4))
    error<-c(error, abs(y[i]-solucion [i, 2]))
    #cat(t[i-1]," | ", y[i-1]," | ",k1," | ",k2," | ",k3," | ",k4," | ", abs(y[i-1]-solucion [i-1, 2]), " | ", solucion[i-1, 2], "\n")
  }
  cat (length(y))
  if (graficar){
    graficarCampoPendiente(min(t), max(t), min(y), max(y), dy, numpendientes, "RK4")
    graficarSolucionNumerica(t, y)
  }
  rta<-list(w=y, t=t, error=error)
}

rungekutta3<-function(dy, ti, tf, y0, h, graficar=TRUE, numpendientes=10){
  t<-seq(ti, tf, h)
  y<-c(y0)
  solucion=solucionODE (dy, t, h, y0)
  error<-c(0)
  #cat("x    |y         |k1         |k2        |k3       |error absoluto\n")
  for(i in 2:length(t)){
    k1=h*dy( t[i-1], y[i-1])
    k2=h*dy(t[i-1]+h/2, y[i-1]+k1*(0.5))
    k3=h*dy(t[i-1]+h, y[i-1]-k1+2*k2)
    y<-c(y, y[i-1]+1/6*(k1+4*k2+k3))
    error<-c(error, abs(y[i]-solucion [i, 2]))
    #cat(t[i-1]," | ", y[i-1]," | ",k1," | ",k2," | ",k3," | ", abs(y[i]-solucion [i, 2]),"\n")
  }
  if (graficar){
    graficarCampoPendiente(min(t), max(t), min(y), max(y), dy, numpendientes, "RK3")
    graficarSolucionNumerica(t, y)
  }
  rta<-list(w=y, t=t, error=error)
}

r<-rungekutta4(function(x, y){x-y}, 0, 2, 1, 0.1)
data.frame (x=r$t, y=r$w, "Error truncamiento"=r$error)

r2<-rungekutta3(function(x, y){x-y}, 0, 2, 1, 0.1)
data.frame (x=r2$t, y=r2$w, "Error truncamiento"=r2$error)
