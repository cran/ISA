\name{boxPstd}
\alias{boxPstd}
\title{Boxplot standard}
\description{
  Versione modificata del Boxplot fornito di default con R.
}
\usage{
boxPstd(vetto, displace = 1.5)
}
\arguments{
  \item{vetto}{vettore numerico di dimensione \emph{n}}
  \item{displace}{scalare positivo che regola l'estensione dei baffi}
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\author{Fabio Frascati \email{fabiofrascati@yahoo.it}, Federico M. Stefanini \email{stefanini@ds.unifi.it}}
\seealso{\code{\link[graphics:boxplot]{boxplot}}, \code{\link[grDevices:boxplot.stats]{boxplot.stats}}, \code{\link[graphics:bxp]{bxp}}}
\examples{
boxPstd(vetto=c(1.0,2.3,4.5,6.7,8.9),displace=1.5)

boxPstd(vetto=c(1.50,6.40,9.60,8.80,8.86,7.80,8.60,8.60),displace=1.5)
}
\keyword{univar}
\concept{ISAcap1}

