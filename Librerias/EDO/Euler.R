list.of.packages <- c("phaseR", "pracma", "graphics")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(phaseR)
library(pracma)
library(graphics)

dy <- function(edo, x, y)
{
  return(eval(edo))
}

metodoEuler <- function(edo, h, x0, y0, xf)
{
  N = (xf - x0) / h
  x = y = numeric(N+1)
  x[1] = x0;
  y[1] = y0;
  i = 1
  while (i <= N)
  {
    x[i+1] = x[i] + h
    y[i+1] = y[i] + (h * dy(edo, x[i], y[i]))
    i = i+1
  }
  return (data.frame(X = x, Y = y))
}


graficarEuler <- function(en, edo)
{
  xlim0 <- en$X[1]
  ylim0 <- en$Y[1]
  xlimN <- en$X[nrow(en)]
  ylimN <- en$Y[nrow(en)]
  
  auxGraf <- function(t, y, parameters)
  {
    a <- parameters[1]
    funcGraf <- a*(dy(f, t, y))
    list(funcGraf)
  }
  
  plot(x = en$X, y = en$Y,
       xlab = "x", ylab = "f(x)",
       main = "metodo de Euler", type = "b",
       col = "orange")
  
  auxGraf.flowField <- flowField(auxGraf,
                                 x = c(xlim0, xlimN), y = c(ylim0, ylimN),
                                 parameters = c(1), system = "one.dim",
                                 points = 10, col = "green",xlab = "x", ylab = "f(x)",
                                 main = "metodo de Euler")
  errorEuler(en)
}


errorEuler <- function(en)
{
  enO <- euler_heun(dy, en$X[1], en$X[nrow(en)], en$Y[1], nrow(en)-1)
  errores <- c()
  for (i in 1:nrow(en))
  {
    errores[i] <- abs(enO$y[i] - en$Y[i])
  }
  
  cat("Los errores absolutos son:","\n")
  errores
}
