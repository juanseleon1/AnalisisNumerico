\name{graficarsimpson}
\alias{graficarsimpson}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
graficarsimpson
}
\description{
Genera la grafica de la funcion integrada por el metodo de Simpson
}
\usage{
graficarSimpson(func, n, a, b, main, ejex, ejey)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{func}{
Funcion a graficar por medio del metodo de Simpson
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
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Imprime una grafica con las aproximaciones hechas mediante el metodo de Simpson
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
La funcion esta definida como
graficarSimpson(func,n,a,b,main,ejex,ejey)
{
  // Cuerpo de la funcion
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
