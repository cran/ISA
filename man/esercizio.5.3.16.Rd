\name{esercizio.5.3.16}
\alias{esercizio.5.3.16}
\docType{data}
\title{Data frame Esercizio 5.3.16}
\description{
  In uno studio sul pH di barattoli di pomodoro è stato pianificato un esperimento
  con 3 fattori sperimentali ognuno a due livelli (assente, presente): conservante, correttore,
  esaltatore. La variabile risposta è il pH (file \dQuote{eseC5.016.R}).
}
\usage{data(esercizio.5.3.16)}
\format{
  Data frame composto di 24 osservazioni nelle seguenti 5 variabili:
  \describe{
    \item{\code{block}}{fattore blocco con livelli \code{1}, \code{2}, \code{3}, \code{4}, \code{5} e \code{6}}
    \item{\code{conservante}}{fattore conservante con livelli \code{0} e \code{1}}
    \item{\code{correttore}}{fattore correttore con livelli \code{0} e \code{1}}
    \item{\code{esaltatore}}{fattore esaltatore con livelli \code{0} e \code{1}}
    \item{\code{pH}}{concentrazione di polifenoli}
  }
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\examples{
data(esercizio.5.3.16)
str(esercizio.5.3.16)
}
\keyword{datasets}
