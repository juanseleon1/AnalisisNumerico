#'Funcion dy
#'
#'Funcion que evalua una expresion dada, utilizando
#'los valores de "x" y "y" que recibe.
#'
#'@param edo La expresion que sera evaluada. DEBE escribirse con la funcion expression de R.
#'@param x El valor de x que sera computado en la funcion
#'@param y El valor de y que sera computado en la funcion
#'
#'@return Ninguno
#'
#'@export
dy <- function(edo, x, y)
{
  return(eval(edo))
}
