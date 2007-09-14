\name{esercizio.5.3.7}
\alias{esercizio.5.3.7}
\docType{data}
\title{Data frame Esercizio 5.3.7}
\description{
  In un esperimento è stata valutata la quantità di grasso Y contenuta in bottiglie standard
  di latte in fuznione della quantità di integratore alimentare X giornalmente consumato dalle
  vacche che lo producono. Si assuma che, sulla scala scelta, Y sia assimilabile ad una variabile
  casuale Gaussiana con media condizionata a X e varianza \eqn{\sigma^2} incognita ma costante. Impiegare
  i risultati contenuti nel file \dQuote{eseC5.007.R}. 
}
\usage{data(esercizio.5.3.7)}
\format{
  Data frame composto di 84 osservazioni nelle seguenti 2 variabili:
  \describe{
    \item{\code{Y}}{quantità di grasso}
    \item{\code{X}}{integratore alimentare}
  }
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\examples{
data(esercizio.5.3.7)
str(esercizio.5.3.7)
}
\keyword{datasets}
