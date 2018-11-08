#'Funcion graficarEuler
#'
#'Funcion que grafica la solucion de una ecuacion diferencial ordinaria
#'de primer orden, el campo vectorial en que esta inmersa, graficando los
#'puntos necesarios, y utiliza la funcion errorEuler para mostrar los
#'errores.Hace uso de una funcion embedida para graficar el campo vectorial.
#'
#'@param en dataframe que contiene los resultados de la funcion metodoEuler
#'@param edo la expresion a evaluar por medio de la funcion dy
#'
#'@return Ninguno
#'
#'@import pracma
#'@import graphics
#'@import phaseR
#'
#'@export
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
