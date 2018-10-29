algoritmo<-function(f,x,y,h,m){
y[1]<-y
x[1]<-x
  for( i in c(1:m))
  {
    k1<-h*f(x[i],y[i])
    k2<-h*f(x[i]+k1,y[i]+k1)
    y[i+1]<-y[i]+0.5*(k1+k2)
    x[i+1]<-x[i]+h
  }
return (data.frame(X = x, Y = y))
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
e1<-algoritmo(dy,0,1,0.1,20)
xx <- c(0,2); yy <- c(1,15)
vectorfield(dy, xx, yy, scale = 0.1)
for (xs in seq(1, 15, by = 0.25)) 
{
  sol <- rk4(dy, 0, 3, xs, 100)
  lines(sol$x, sol$y, col="purple")
}
curve(fy,add = T,col="Red")
xe<-e1[,1]
aux<-e1[,2]
aux2<-fy(c(seq(0,2,0.1)))
cat("x \tValor Real \t\tValor Estimado \t\tError\n")
for(i in c(0:length(aux)))
{
  cat(xe[i],"\t",aux2[i],"\t\t",aux[i],"\t\t",abs(aux[i]-aux2[i]),"\n")
}