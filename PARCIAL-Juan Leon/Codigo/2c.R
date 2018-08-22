intersec<-function(f,E,xo,x1)
{
  xna<-x1
  xn<-xo
  xi<-0
  cont=1
  repeat
  {
    xi<-xn
    frac<-(xn-xna)/(f(xn)-f(xna))
    xn<-xn-(f(xn)*frac)
    xna<-xi
    d<-abs(xn-xna)
    if(d<E)
      break
  }
  cat("El punto de interseccion es: (",toString(format(xn,nsmall = 7)),",",toString(format(log(xn+2),nsmall = 7)),")\n")
}

f<-function(x)
{
  return(sin(x)-log(x+2))
}
intersec(f,10**-7,-1.9,-1)