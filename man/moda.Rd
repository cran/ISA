\name{moda}
\alias{moda}
\title{Moda}
\description{
  Valore che corrisponde alla frequenza assoluta massima.
}
\usage{
moda(vect)
}
\arguments{
  \item{vect}{vettore alfanumerico di dimensione \emph{n}}
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\author{Fabio Frascati \email{fabiofrascati@yahoo.it}, Federico M. Stefanini \email{stefanini@ds.unifi.it}}
\seealso{\code{\link[base:mean]{mean}}, \code{\link[stats:median]{median}}}
\examples{
moda(vect=c(1.3,1.3,1.3,2.7,5.1,5.1))

moda(vect=c("a","c","b","c","a","c","b","c","a","a","c","b","c"))
}
\keyword{univar}
\concept{ISAcap1}

