aprox<-function(xo=1.8)
{
  x<-c(1,1.1,1.2,1.3)
  y<-c(2.7183,3.3046,3.9841,4.7701)
  fa<-splinefun(x,y)
  curve(fa)
  h<-c(10**-1,10**-2,10**-3,10**-4,10**-5)
  df<-c()
  interx<-c(xo)
  intery<-c(fa(xo))
  for(i in c(1:length(h)))
  {
    a<-fa(xo+h[i])-fa(xo)
    a<-a/h[i]
    interx<-c(interx,xo+h[i])
    intery<-c(intery,fa(xo+h[i]))
    df<-c(df,a)
    cat("h= ",h[i], "df= ",a,"\n")
  }
  cat(length(interx)," ",length(intery),"\n")
  ffd<-splinefun(interx,intery)
  e<-c()
  max<-0
  for (i in c(1:(length(interx)-1)))
  {
    ex<-(h[i]/2)*ffd(interx[i])
    e<-c(e,ex)
    if(ex>max)
      max<-ex
  }
  print(e)
  cat("Cota: ",max, "\n")
  
}
aprox()