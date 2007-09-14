\name{esercizio.5.3.3}
\alias{esercizio.5.3.3}
\docType{data}
\title{Data frame Esercizio 5.3.3}
\description{
  In uno studio inerente la concentrazione di composti aromatici in reazioni di fermentazione
  sono state considerate le variabili seguenti: concentrazione finale (Y), temperatura (X1 con -1
  per bassa e +1 per alta), pressione (X2 con -1 per bassa e +1 per alta). Un campione casuale di
  444 reazioni in condizioni controllate ha dato luogo ai risultati contenuti nel file \dQuote{ese.C5.003.R}.  
}
\usage{data(esercizio.5.3.3)}
\format{
  Data frame composto di 444 osservazioni nelle seguenti 3 variabili:
  \describe{
    \item{\code{Y}}{concentrazione finale}
    \item{\code{X1}}{temperatura}
    \item{\code{X2}}{pressione}
  }
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\examples{
data(esercizio.5.3.3)
str(esercizio.5.3.3)
}
\keyword{datasets}
