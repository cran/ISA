\name{esercizio.4.4.13}
\alias{esercizio.4.4.13}
\docType{data}
\title{Data frame Esercizio 4.4.13}
\description{
  In uno studio sul contenuto di una certa molecola aromatica in frutti selvatici
  vengono esaminati 6 campioni casuali di provenienza diversa per altitudine (A)
  ed esposizione al sole (E). I livelli sono: A1 per altitudine base, A2 per alta
  e A3 per molto alta; inoltre S1 indica una esposizione bassa e S2 una esposizione
  normale. La concentrazione X è una variabile casuale Normale con varianza costante
  nei 6 livelli.
}
\usage{data(esercizio.4.4.13)}
\format{
  Data frame composto di 18 osservazioni nelle seguenti 4 variabili:
  \describe{
    \item{\code{Y}}{concentrazione}
    \item{\code{A}}{fattore altitudine con livelli \code{1}, \code{2} e \code{3}}
    \item{\code{S}}{fattore esposizione con livelli \code{1} e \code{2}}
    \item{\code{code}}{fattore codice con livelli \code{A1S1}, \code{A1S2}, \code{A2S1}, \code{A2S2}, \code{A3S1} e \code{A3S2}}
  }
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\examples{
data(esercizio.4.4.13)
str(esercizio.4.4.13)
}
\keyword{datasets}
