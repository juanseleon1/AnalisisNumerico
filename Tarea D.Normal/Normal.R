z<-function(x)
{
  b=(1/(sqrt(2*pi)))
  a=(exp((-(x**2)/(2))))
  return(a*b)
}
area<- function(a,b,d) {
  vx<-seq(a,b,by=d)
  v1<-c(0,vx)
  v2<-c(vx,0)
  va<-((v1+v2)/2)
  vy<-z(va)
  vy<-vy[2:(length(va)-1)]
  dx<-c()
  dx[1:length(vy)]<-d
  q<-dx*vy
  resul<-sum(q)
  return(resul)
}
Z<-function(x)
{
  if(x>0)
  {
    return(area(0,x,(x/1000)))
  }else
  {
    return(area(x,0,(-x/1000)))
  }
}
tabla<-function(a,b)
{
  enc<-seq(0,0.00009,by=0.00001)
  cat("Z\t")
  for(i in c(1:(length(enc)-1)))
  {
    cat(enc[i],"\t\t")
  }
  cat("\n")
  cont<-seq(a,b,by=0.0001)
  for(i in c(1:(length(cont)-1)))
  {
    if(i%%10==1)
    {
      cat("\n")
      cat(toString(format(cont[i],nsmall = 4)),"\t",toString(format(round(Z(cont[i])),nsmall = 5)),"\t")
    }
    else
    {
      cat(toString(format(round(Z(cont[i]),5),nsmall = 5)),"\t")
    }
    
    
  }
}
curve(z,xlim = c(-4,4),ylim=c(0,0.4))
area(-1,1,0.1)
tabla(0,1)