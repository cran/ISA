\name{media}
\alias{media}
\title{Media aritmetica}
\description{
  Media aritmetica semplice.
}
\usage{
media(v1)
}
\arguments{
  \item{v1}{vettore numerico di dimensione \emph{n}}
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\author{Fabio Frascati \email{fabiofrascati@yahoo.it}, Federico M. Stefanini \email{stefanini@ds.unifi.it}}
\seealso{\code{\link[base:mean]{mean}}, \code{\link[stats:median]{median}}, \code{\link[stats:weighted.mean]{weighted.mean}}}
\examples{
media(v1=c(1.3,1.3,1.3,2.7,5.1,5.1))

media(v1=c(1.0,2.3,4.5,6.7,8.9))
}
\keyword{univar}
\concept{ISAcap1}
