\name{esercizio.5.3.1}
\alias{esercizio.5.3.1}
\docType{data}
\title{Data frame Esercizio 5.3.1}
\description{
  Si consideri uno studio in cui è stata valutata la concentrazione Y di una certa proteina
  estratta da cellule in 16 piastre di coltura (linea cellulare HTC122). Le condizioni di incubazione
  delle piastre differiscono per quanto riguarda la temperatura, X1, e per la presenza o assenza di una
  certa vitamina, X2.
}
\usage{data(esercizio.5.3.1)}
\format{
  Data frame composto di 16 osservazioni nelle seguenti 3 variabili:
  \describe{
    \item{\code{tempe}}{temperatura}
    \item{\code{vita}}{vitamina}
    \item{\code{prote}}{proteina}
  }
}
\details{
  Data frame corretto.
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\examples{
data(esercizio.5.3.1)
str(esercizio.5.3.1)
}
\keyword{datasets}
