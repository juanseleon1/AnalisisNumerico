#'Funcion errorEuler
#'
#'Funcion que calcula los resultados exactos de una ecuacion diferencial
#'ordinaria de primer orden por medio del metodo de Euler disponible en
#'R, hayando el error absoluto a comparacion de los resultaods obtenidos
#'por medio de la funcion metodoEuler
#'
#'@param en dataframe que contiene los resultados de la funcion metodoEuler
#'
#'@return Ninguno
#'
#'@import pracma
#'
#'@export
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
