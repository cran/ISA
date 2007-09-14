\name{difSempliceMed}
\alias{difSempliceMed}
\title{Differenza semplice media}
\description{
  Quantità riassuntiva complessiva basata sulle differenze tra valori
  dello stesso vettore.
}
\usage{
difSempliceMed(vetto)
}
\arguments{
  \item{vetto}{vettore numerico di dimensione \emph{n}}
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\author{Fabio Frascati \email{fabiofrascati@yahoo.it}, Federico M. Stefanini \email{stefanini@ds.unifi.it}}
\seealso{\code{\link[base:range]{range}}, \code{\link[base:Extremes]{min}}, \code{\link[base:Extremes]{max}}}
\examples{
difSempliceMed(vetto=c(1.3,1.3,1.3,2.7,5.1,5.1))

difSempliceMed(vetto=c(1.0,2.3,4.5,6.7,8.9))
}
\keyword{univar}
\concept{ISAcap1}

