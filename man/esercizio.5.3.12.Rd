\name{esercizio.5.3.12}
\alias{esercizio.5.3.12}
\docType{data}
\title{Data frame Esercizio 5.3.12}
\description{
  In uno studio inerente un processo fermentativo si considera la concentrazione finale Y di polifenoli
  in relazione alla concentrazione iniziale di zuccheri X1, alla concentrazione iniziale di azoto X2, e alla
  concentrazione iniziale X3 della molecola CZ15 (file di dati \dQuote{eseC5.012.R}). Assumere che il vettore
  \emph{epsilon} abbia elementi i.i.d. distribuiti come una Normale con varianza costante.
}
\usage{data(esercizio.5.3.12)}
\format{
  Data frame composto di 100 osservazioni nelle seguenti 4 variabili:
  \describe{
    \item{\code{Y}}{concentrazione finale di polifenoli}
    \item{\code{X1}}{concentrazione iniziale di zuccheri}
    \item{\code{X2}}{concentrazione iniziale di azoto}
    \item{\code{X3}}{concentrazione iniziale della molecola CZ15}
  }
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\examples{
data(esercizio.5.3.12)
str(esercizio.5.3.12)
}
\keyword{datasets}
