#'Funcion metodoEuler
#'
#'Funcion que resuelve ecuaciones diferenciales ordinarias
#'de primer orden, por medio del metodo numerico de Euler.
#'Esta funcion en particular UNICAMENTE puede utilizarse para
#'problemas de valor inicial.
#'
#'@param edo El nombre de la expresion a evaluar.
#'@param h El tamanio del paso entre los valores de x. NO MENORES A 1e-6
#'@param x0 El valor inicial de x
#'@param y0 El valor inicial de y al ser evaluada en x0
#'@param xf El valor final de x para el ejercicio
#'
#'@return dataframe con los valores calculados de x, acorde a h, y y
#'
#'@export
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
