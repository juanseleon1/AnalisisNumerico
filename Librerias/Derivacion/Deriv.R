f<-function(x)
{
  return(x*(exp(x)))
}
fp<-function(x,h,f)
{
  a<-((1/(2*h))*(f(x+h)-f(x-h)))
  return(a)
}
fp1<-function(x,h,f)
{
  a<-((1/(2*h))*(-3*f(x)+4*f(x+h)-f(x+2*h)))
  return(a)
}
yp<-function(x)
{
  return(exp(x)+(x*exp(x)))
}

x=c(1.8,1.9,2,2.1,2.2)
x
h<-c(0.1,0.01,0.001,0.0001)
y<-f(x)
der<-yp(x)

cat("Evaluando:\nx\ty\n")
for(i in 1:length(x))
{
cat(x[i],"\t",y[i],"\n")
}
cat("Evaluando en la derivada exacta:\nx\ty'\n")
for(i in 1:length(x))
{
  cat(x[i],"\t",der[i],"\n")
}
cat("\n\nAproximacion (4.4):\n")
cat("h\tx\ty\n")
aux2<-c()
er1<-c()
for(i in 1:length(x))
{
  for(j in 1:length(h))
  {
    l<-fp1(x[i],h[j],f)
    aux2<-c(aux2,l)
    e<-der[i]-l
    er1<-c(er1,e)
    cat(h[j],"\t",x[i],"\t",l,"\n")
  }
}
cat("\nError (4.4):\n")
cat("h\tx\tError\n")
for(i in 1:length(x))
{
  for(j in 1:length(h))
  {
    aux1<-aux2[(1*i):(4*i)]
    cat(h[j],"\t",x[i],"\t",er1[j],"\t\n")
  }
  
}
cat("\n\nAproximacion (4.5):\n")
cat("h\tx\ty\n")
aux<-c()
er<-c()
for(i in 1:length(x))
{
  for(j in 1:length(h))
  {
    l<-fp(x[i],h[j],f)
    aux<-c(aux,l)
    e<-der[i]-l
    er<-c(er,e)
    cat(h[j],"\t",x[i],"\t",l,"\n")
  }
}
cat("\nError (4.5):\n")
cat("h\tx\tError\n")
for(i in 1:length(x))
{
  for(j in 1:length(h))
  {
    cat(h[j],"\t",x[i],"\t",er[j],"\t\n")
  }
  
}

plot(h,er[5:8])
