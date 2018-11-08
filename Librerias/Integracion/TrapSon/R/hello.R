# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

options(warn=-1)
require(ggplot2)
require(rSymPy)

hello <- function() {
  print("Hello, world!")
}

lagrange.poly <- function(x, y) {

  l <- list() #
  k <- 1

  for (i in x) {
    num <- 1
    denom <- 1

    p <- x[! x %in% i]
    for (j in p) {
      num <- paste(num, "*", "(", 'x', " - ", as.character(j), ")", sep = ", collapse = ")
      denom <- paste(denom, "*", "(", as.character(i)," - ", as.character(j), ")", sep = ", collapse = ")
    }

    l[k] <- paste("(", num, ")", "/", "(", denom, ")", sep = ", collapse = ")
    k <- k + 1
  }

  eq <- 0

  for (i in 1:length(y)) {
    eq <- paste(eq, '+', as.character(y[i]), "*", l[[i]], sep = ", collapse = ")
  }
  #x <- Var('x')
  return(eq)
}

cube <- function(x,titulo=NULL,ejex=NULL, ejey=NULL){
  plot(x^3, main=titulo, xlab=ejex, ylab=ejey)
  return(x^3)
}

integral <- function(x,lim1,lim2){
  print(integrate(x,lim1,lim2))
}

cuadrado <- function(x){
  return(x^2)
}

graficarsimpson<- function(func, n, a, b,titulo=NULL,ejex=NULL, ejey=NULL){
  df <- data.frame(cbind(c(a, b, b, a), c(a, func(b), a, a)))
  curvedf <- data.frame(cbind(c(a, a, b), c(a, func(a), func(b))))
  var = as.numeric(lagrange.poly(curvedf$X1, curvedf$X2))
  #var = poly.calc(curvedf$X1, curvedf$X2)
  f2 <- function(x) {
  return(var)
  }
  ggplot(data = df) +
    ggtitle(titulo) + xlab(ejex) + ylab(ejey) +
    stat_function(fun = func, color = 'blue') + xlim(c(a,b)) +
    stat_function(fun = f2) +
    geom_segment(aes(x = 0, y = 0, xend = b, yend = func(b))) +
    geom_segment(aes(x = b, y = 0, xend = b, yend = func(b))) +
    geom_polygon(data = df, aes(x=X1, y=X2), fill = 'blue', alpha = 0.2) +
    geom_area(stat = 'function', fun = f2, fill = 'black', alpha = 0.4, xlim = c(a,b))

}

simpson = function(fun, a,b, n) {
  intval <- integrate(fun,a,b)
  val2 = intval$value
  if (n%%2 != 0) stop("En la regla de Simpson, n es par!")
  h = (b-a)/n
  i1 = seq(1, n-1, by = 2) # impares
  i2 = seq(2, n-2, by = 2) # pares
  y = fun(a+(0:n)*h) # f(a), f(a+h),...,f(a+i*h),...
  abs(h/3 * ( fun(a) + fun(b) + 4*sum(y[i1]) + 2*sum(sum(y[i2]) ) ))
  suma = abs(h/3 * ( fun(a) + fun(b) + 4*sum(y[i1]) + 2*sum(sum(y[i2]) ) ))
  error = val2-suma
  cat("Integral Simpson: ",suma, "\n")
  cat("Error Simpson: ",error, "\n")
}

graficartrap<- function(func, n, a, b, titulo=NULL,ejex=NULL, ejey=NULL){
  if(a==0){
    seg <- seq.int(a+0.001, b, length.out = n)
  }
  else{
    seg <- seq.int(a, b, length.out = n)
  }
  fx <- vector(length = length(seg))
  for (i in 1:length(seg)) {
    fx[i] <- func(seg[i])
  }
  df <- data.frame(xend = seg,
                   y = rep(0, n),
                   yend = fx,
                   yend1 = c(fx[2:n], fx[n]),
                   xend1 = c(seg[2:n], seg[n]))
  ggplot(data = df) +
    ggtitle(titulo) + xlab(ejex) + ylab(ejey) +
    stat_function(fun = func, size = 1.05, alpha = 0.75, color='blue') +
    geom_segment(aes(x=xend, y=y, xend=xend, yend=yend)) +
    geom_ribbon(aes(x=xend, ymin=y, ymax=yend), fill = 'black', alpha = 0.3) +
    xlim(c(a, b))
}
trapezoid <- function(func, a, b, n) {


  intval <- integrate(func,a,b)
  val2 = intval$value
  h <- (b-a)/n
  x <- seq(a, b, by=h)
  y <- func(x)
  s <- h * (abs(y[1]/2) + abs(sum(y[2:n])) + abs(y[n+1]/2))
  suma <- h * (abs(y[1]/2) + abs(sum(y[2:n])) + abs(y[n+1]/2))
  cat("Integral trapecio: ",suma,"\n")
  error = abs(val2-suma)
  if (error == 0) {
  cat("Error trapecio: 0 ","\n")
  } else {
  cat("Error trapecio: ",error,"\n")
  }

}


