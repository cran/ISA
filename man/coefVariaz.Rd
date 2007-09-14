\name{coefVariaz}
\alias{coefVariaz}
\title{Coefficiente di variazione}
\description{
  Rapporto tra scarto quadratico medio e valore assoluto della media.
}
\usage{
coefVariaz(vettoVal)
}
\arguments{
  \item{vettoVal}{vettore numerico di dimensione \emph{n}}
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\author{Fabio Frascati \email{fabiofrascati@yahoo.it}, Federico M. Stefanini \email{stefanini@ds.unifi.it}}
\seealso{\code{\link[base:mean]{mean}}, \code{\link[stats:sd]{sd}}, \code{\link[stats:cor]{var}}}
\examples{
coefVariaz(vettoVal=c(1.3,1.3,1.3,2.7,5.1,5.1))

coefVariaz(vettoVal=c(1.0,2.3,4.5,6.7,8.9))
}
\keyword{univar}
\concept{ISAcap1}

