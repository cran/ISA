\name{FLinearMod}
\alias{FLinearMod}
\title{Minimi quadrati ordinari}
\description{
  Stime dei minimi quadrati, residui, valori previsti dal modello, valori di leva e stima corretta della varianza degli errori.
}
\usage{
FLinearMod(Y, X)
}
\arguments{
  \item{Y}{matrice della variabile dipendente (\emph{n} righe e \emph{1} colonna)}
  \item{X}{matrice del modello (\emph{n} righe e \emph{k} colonne)}
}
\value{
  \item{Y }{matrice della variabile dipendente}
  \item{X }{matrice del modello}
  \item{XTX }{prodotto matriciale incrociato}
  \item{detXTX }{determinante del prodotto matriciale incrociato}
  \item{inversaXTX }{inversa del prodotto matriciale incrociato}
  \item{XTY }{prodotto matriciale tra la matrice del modello e quella della variabile dipendente}
  \item{beta }{stime minimi quadrati ordinari}
  \item{H }{matrice di proiezione}
  \item{IH }{differenza tra la matrice identità di dimensione \emph{n} e la matrice di proiezione H}
  \item{attesa }{valori attesi secondo il modello}
  \item{residui }{residui di regressione}
  \item{varianzaStimata }{stima corretta della varianza degli errori}
}
\references{
 Stefanini, F. M.
 \emph{INTRODUZIONE ALLA STATISTICA APPLICATA con esempi in R.}
 PEARSON Education Milano, 2007, \url{http://hpe.pearsoned.it/stefanini}.
}
\author{Fabio Frascati \email{fabiofrascati@yahoo.it}, Federico M. Stefanini \email{stefanini@ds.unifi.it}}
\seealso{\code{\link[stats:lm]{lm}}, \code{\link[stats:summary.lm]{summary.lm}}, \code{\link[stats:lmfit]{lmfit}}, \code{\link[stats:lsfit]{lsfit}}, \code{\link[stats:residuals]{residuals}}, \code{\link[stats:fitted.values]{fitted}}, \code{\link[stats:influence.measures]{hatvalues}}}
\examples{
   y<-c(1.50,6.40,9.60,8.80,8.86,7.80,8.60,8.60)
   x1<-c(1.1,2.3,4.5,6.7,8.9,3.4,5.6,6.7)
   x2<-c(1.2,3.4,5.6,7.5,7.5,6.7,8.6,7.6)
   x3<-c(1.40,5.60,7.56,6.00,5.40,6.60,8.70,8.70)
   FLinearMod(Y=y,X=cbind(1,x1,x2,x3))
}
\keyword{regression}
\concept{ISAcap5}

