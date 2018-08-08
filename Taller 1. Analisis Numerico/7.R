#Por: Maria Garces && Juan Leon
f<-function(x){
  return(sqrt((2-(2*cospi(x)))**2+(1-sinpi(x))**2))
}
df<-function(x)
{
  return(((4*sinpi(x) )- (((3*sinpi(x)) +1)*cospi(x)))/(sqrt((sinpi(x) * sinpi(x))-(2*sinpi(x))+(4*(cospi(x) * cospi(x)))-(8*cospi(x))+5)))
}
dat<- data.frame(t=seq(0, 3*pi, by=0.1) )
xhrt <- function(t) 2*cos(t)
yhrt <- function(t) sin(t)
dat$y=yhrt(dat$t)
dat$x=xhrt(dat$t)
with(dat, plot(x,y, type="l",col="Green"))
newton<-function(f,df,E,xo)
{
  k=0
  repeat{
    c=f(xo)/df(xo)
    x1=xo-c
    dx=abs(c)
    xo=x1
    k=k+1
    if(dx<E ||k>100)
      break;
  }
  y1<-sin(x1)
  cat("# de iteraciones: ",k," El valor de xmax: ",format(x1,nsmall = 4),"rad, f(x): ",f(x1), ",Error Estimado: ", c)
  cat("\nEl punto es: (",format(2*cos(x1),nsmall = 4),",",format(y1,nsmall = 4),", 0.0000)\n")
  points(x=2,y=1,pch=19)
  points(x=2*cos(x1),y=y1,pch=19,col="Red")
}
newton(f,df,0.0001,1)