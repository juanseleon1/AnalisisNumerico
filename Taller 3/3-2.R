library(PolynomF)
library(rSymPy)
#Se debe instalar RSymPy
lagrange.poly <- function(x, y) {
  
  l <- list() # List to store Lagrangian polynomials L_{1,2,3,4}
  k <- 1
  
  for (i in x) {
    # Set the numerator and denominator of the Lagrangian polynomials to 1 and build them up
    num <- 1
    denom <- 1
    
    # Remove the current x value from the iterated list
    p <- x[! x %in% i]
    
    # For the remaining points, construct the Lagrangian polynomial by successively 
    # appending each x value
    for (j in p) {
      num <- paste(num, "*", "(", 'x', " - ", as.character(j), ")", sep = "", collapse = "")
      denom <- paste(denom, "*", "(", as.character(i)," - ", as.character(j), ")", sep = "", collapse = "")
    }
    
    # Set each Lagrangian polynomial in rSymPy to simplify later.
    l[k] <- paste("(", num, ")", "/", "(", denom, ")", sep = "", collapse = "")
    k <- k + 1
  }
  
  # Similar to before, we construct the final Lagrangian polynomial by successively building 
  # up the equation by iterating through the polynomials L_{1,2,3,4} and the y values 
  # corresponding to the x values.
  eq <- 0
  
  for (i in 1:length(y)) {
    eq <- paste(eq, '+', as.character(y[i]), "*", l[[i]], sep = "", collapse = "")
  }
  
  # Define x variable for rSymPy to simplify
  x <- Var('x')
  
  # Simplify the result with rSymPy and return the polynomial
  return(sympy(paste("simplify(", eq, ")")))
}
c<-c(100,200,300,400,500,600)
y<-c(-160,-35,-4.2,9,16.9,21.3)
plot(c,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Diagrama")
re=lagrange.poly(c,y)
Ajuste_Polinomio = poly.calc(c,y)
curve(Ajuste_Polinomio,add=T,from = 100, to = 700)
f <- as.function(alist(x=, eval(parse(text=re))))
curve(f,add=T,from = 100, to = 700,col="Blue")
cat("Ajuste polinomico: en negro. Lagrange: Azul \n")
cat("Con Lagrange: \nEl polinomio interpolante es: ",re,"\n")
cat("Cuando T=450K, B tiene un valor de: ",f(450),"\n")
cat("Con ajuste Polinomico: \nEl polinomio interpolante es: ",as.character(Ajuste_Polinomio),"\n")
cat("Cuando T=450K, B tiene un valor de: ",Ajuste_Polinomio(450),"\n")
