\name{esercizio.5.3.8}
\alias{esercizio.5.3.8}
\docType{data}
\title{Data frame Esercizio 5.3.8}
\description{
  In una prova in serra si valuta l'area di infezione fogliare Y nel clone
  Kruza (patata). Un campione casuale di 3 piante non è stato infettato, un
  secondo campione di 3 piante è stato infettato con una dose singola di inoculo, il
  terzo campione di 3 piante è stato infettato con una doppia dose di inoculo. Si assuma
  che l'area di infezione sia assimilabile ad una variabile casuale Normale e che la varianza
  sia costante.  
}
\usage{data(esercizio.5.3.8)}
\format{
  Data frame composto di 9 osservazioni nelle seguenti 2 variabili:
  \describe{
    \item{\code{Y}}{infezione fogliare}
    \item{\code{dose}}{fattore dose con livelli \code{0}, \code{1} e \code{2}}
  }
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\examples{
data(esercizio.5.3.8)
str(esercizio.5.3.8)
}
\keyword{datasets}
