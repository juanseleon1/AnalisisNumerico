a<-function(x)
{
  
  return(abs(sin(x)-cos(x)))
}

areaT<- function(a,b,n,z) {
  d<-(b-a)/n
  vx<-seq(a,b,by=d)
  vy<-z(vx)
  v1<-c(0,vy)
  v2<-c(vy,0)
  va<-((v1+v2)/2)
  vy<-vy[2:(length(va)-1)]
  dx<-c()
  q<-d*vy
  resul<-sum(q)
  draw(a,b)
  return(resul)
}
areaR<- function(a,b,n,z) {
  d<-(b-a)/n
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
  draw(a,b)
  return(resul)
}
areaER<- function(a,b,n,z1,z2) {
  d<-(b-a)/n
  vx<-seq(a,b,by=d)
  v1<-c(0,vx)
  v2<-c(vx,0)
  va<-((v1+v2)/2)
  vy1<-z1(va)
  vy2<-z2(va)
  vy<-c()
  for(i in c(2:(length(va)-1)))
  {
    if(vy1[i]>vy2[i])
    {
      vy<-c(vy,vy2[i])
    }else
    {
      vy<-c(vy,vy1[i])
    }
  }
  q<-d*vy
  resul<-sum(q)
  drawP(a,b)
  return(resul)
}
draw<-function(a,b)
{
  cord.x<-c(seq(a,b,0.01),seq(b,a,-0.01))
  q<-sin(seq(a,b,0.01))
  w<-cos(seq(a,b,0.01))
  y<-c()
  yy<-c()
  for(i in c(1:length(q)))
  {
    if(q[i]>w[i])
    {
      y<-c(y,q[i])
      yy<-c(w[i],yy)
    }
    else
    {
      y<-c(y,w[i])
      yy<-c(q[i],yy)
    }
  }
  cord.y<-c(y,yy)
  polygon(cord.x,cord.y,col="skyblue")
}
drawP<-function(a,b)
{
  cord.x<-c(seq(a,b,0.01))
  q<-sin(seq(a,b,0.01))
  w<-cos(seq(a,b,0.01))
  yy<-c()
  for(i in c(1:length(q)))
  {
    if(q[i]>w[i])
    {
      yy<-c(yy,w[i])
    }
    else
    {
      yy<-c(yy,q[i])
    }
  }
  cord.y<-c(yy)
  polygon(cord.x,cord.y,col="gray")
}
curve(sin,xlim = c(0,2*pi),ylim= c(-1,1),col="Red")
curve(cos,xlim = c(0,2*pi),ylim= c(-1,1),col="Blue",add=T)
abline(h=0,lwd=1,col="Black")
print(areaR(0,pi/2,10,a))
# print(areaT(1.5,4,10,a))
print(areaER(0,(pi/2),10,sin,cos))

