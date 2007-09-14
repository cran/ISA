\name{esercizio.4.4.14}
\alias{esercizio.4.4.14}
\docType{data}
\title{Data frame Esercizio 4.4.14}
\description{
  In un'indagine campionaria svolta in alcuni supermercati un campione
  di clienti ha espresso il grado di preferenza per le cipolle in agrodolce. LA
  variabile grado di preferenza ha valori: nullo (X = 0), basso (X = 1), medio (X = 10),
  alto (X = 100), altissimo (X = 5000). La distribuzione delle frequenze assolute
  per sesso (Y = 1 se femmine) è rappresentata in questo stesso data frame.
}
\usage{data(esercizio.4.4.14)}
\format{
  Data frame composto di 10 osservazioni nelle seguenti 3 variabili:
  \describe{
    \item{\code{Y}}{sesso}
    \item{\code{X}}{grado di preferenza}
    \item{\code{frequenze}}{frequenze assolute}
  }
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\examples{
data(esercizio.4.4.14)
str(esercizio.4.4.14)
}
\keyword{datasets}
