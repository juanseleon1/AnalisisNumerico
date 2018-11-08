#iNTEGRACION
#SIMPSON
simpson = function(fun, a,b, n) {
  if (n%%2 != 0) stop("En la regla de Simpson, n es par!")
  h = (b-a)/n
  i1 = seq(1, n-1, by = 2) # impares
  i2 = seq(2, n-2, by = 2) # pares
  h/3 * ( fun(a) + fun(b) + 4*sum( fun(a+i1*h) ) + 2*sum( fun(a+i2*h) ) )
}
##--------------------------------------------------------------------------
f = function(x) exp(-x) * cos(x)
simpson(f,0,pi,10)
##--------------------------------------------------------------
##fUNCION INTEGRATE
f = function(x) exp(-x) * cos(x)
integrate(f, 0, pi)
# Vectorize() si f opera con vectores
f1 = function(x) max(0, x)
h = Vectorize(f1)
integrate(h, -1, 1)
##gAUSS
fgauss = function(t) exp(-t^2/2)
integrate(fgauss, -Inf, Inf)
integrate(fgauss, -Inf, Inf)$value
##FUNCION CON CEROS
st = function(t) t-sin(t^2)-1
t=c(0,pi/8,pi/4,pi/2,pi,2*pi)
plot(st)
S = function(x) integrate(st, 0, x)$value
# Graficar S(x)-----
h = Vectorize(S)
curve(h, 0,3)
#-------------------
require(pracma)
newtonRaphson(S, 2.5)
