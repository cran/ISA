\name{fiveNumbersSummary}
\alias{fiveNumbersSummary}
\title{Cinque numeri di Tukey}
\description{
  minimo, primo quartile, mediana, terzo quartile e massimo.
}
\usage{
fiveNumbersSummary(vx)
}
\arguments{
  \item{vx}{vettore numerico di dimensione \emph{n}}
}
\value{
  \item{minimo }{valore minimo}
  \item{quant0.25 }{primo quartile}
  \item{mediana }{mediana}
  \item{quant0.75 }{terzo quartile}
  \item{massimo }{valore massimo}
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\author{Fabio Frascati \email{fabiofrascati@yahoo.it}, Federico M. Stefanini \email{stefanini@ds.unifi.it}}
\seealso{\code{\link[stats:fivenum]{fivenum}}, \code{\link[base:summary]{summary}}, \code{\link[stats:quantile]{quantile}}}
\examples{
fiveNumbersSummary(vx=c(1.3,1.3,1.3,2.7,5.1,5.1))

fiveNumbersSummary(vx=c(1.0,2.3,4.5,6.7,8.9))
}
\keyword{univar}
\concept{ISAcap1}

