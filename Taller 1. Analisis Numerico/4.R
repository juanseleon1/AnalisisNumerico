#Por: Maria Garces && Juan Leon
cat("Se ingresara el numero: 536.78\n")
num=536.78
n=num
k=0
while(n>10)
{
  n=as.numeric(n/10)
  k=k+1
}
m=4
cat("El valor de n es: ",k, " y la cantidad de cifras que se pueden almacenar es de 4\n")
Em=-10**(k-m)
Ema=10**(k-m)
cat("El error esta entre: [",Em,",",Ema,"]\n")
