#Por: Maria Garces && Juan Leon
n=as.numeric(readline("Ingrese n:"))
i=0
while(n>0)
{
  d<-n%%2
  n<-n/2
  print(d)
  i=i+1
}
cat("T(n)=",i,"\n O(n)= log2(n)")