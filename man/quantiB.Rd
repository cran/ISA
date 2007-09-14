\name{quantiB}
\alias{quantiB}
\title{Quantili}
\description{
  Calcolo dei quantili.
}
\usage{
quantiB(vettore, prob = 0.5)
}
\arguments{
  \item{vettore}{vettore numerico di dimensione \emph{n}}
  \item{prob}{valore di probabilità}
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\author{Fabio Frascati \email{fabiofrascati@yahoo.it}, Federico M. Stefanini \email{stefanini@ds.unifi.it}}
\seealso{\code{\link[stats:quantile]{quantile}}, \code{\link[stats:median]{median}}, \code{\link[base:summary]{summary}}, \code{\link[stats:fivenum]{fivenum}}}
\examples{
quantiB(vettore=c(1.3,1.3,1.3,2.7,5.1,5.1),prob=0.75)

quantiB(vettore=c(1.0,2.3,4.5,6.7,8.9),prob=.25)
}
\keyword{univar}
\concept{ISAcap1}

