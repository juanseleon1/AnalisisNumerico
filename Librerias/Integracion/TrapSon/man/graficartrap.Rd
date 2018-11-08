\name{graficartrap}
\alias{graficartrap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
graficartrap
}
\description{
Genera la grafica de los trapecios correspondientes al calculo de la integral dada
}
\usage{
graficartrap(func, n, a, b, main, ejex, ejey)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{func}{
Funcion a graficar por medio del metodo del trapecio
}
 \item{n}{
Numero de particiones para graficar
}
  \item{a}{
Limite inferior de la integral
}
  \item{b}{
Limite superior de la integral
}

# Argumentos opcionales

 \item{main}{
Titulo de la grafica
}
\item{ejex}{
Eje horizontal de la grafica
}
\item{ejey}{
Eje vertical de la grafica
}

}
\details{
Instalar la libreria "ggplot2" para generar los graficos
}
\value{
Genera la grafica de los trapecios correspondientes a la integral
}
\references{
github.com/valentinaescobar31/AnalisisNumerico
}
\author{
Valentina Escobar, Pedro Escobar
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## La funcion esta definida como:
graficartrap (func, n, a, b, main, ejex, ejey)
{
    if (a == 0) {
        seg <- seq.int(a + 0.001, b, length.out = n)
    }
    else {
        seg <- seq.int(a, b, length.out = n)
    }
    fx <- vector(length = length(seg))
    for (i in 1:length(seg)) {
        fx[i] <- func(seg[i])
    }
    df <- data.frame(xend = seg, y = rep(0, n), yend = fx, yend1 = c(fx[2:n],
        fx[n]), xend1 = c(seg[2:n], seg[n]))
    ggplot(data = df) + stat_function(fun = func, size = 1.05,
        alpha = 0.75, color = "blue") + geom_segment(aes(x = xend,
        y = y, xend = xend, yend = yend)) + geom_ribbon(aes(x = xend,
        ymin = y, ymax = yend), fill = "black", alpha = 0.3) +
        xlim(c(a, b))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
