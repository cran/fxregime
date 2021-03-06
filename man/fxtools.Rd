\name{fxtools}
\alias{fxpegtest}

\title{Various Tools for Exchange Rate Regime Classification}

\description{
Tools for exchange rate regime classification, currently under development.
}

\usage{
  fxpegtest(model, peg = NULL, \dots)
}

\arguments{
  \item{model}{an object of class \code{"fxlm"} as returned by \code{\link[fxregime]{fxlm}}.}
  \item{peg}{character with the name of the currency the target currency is pegged to.
    By default this is chosen to be the currency with the maximal absolute coefficient.}
  \item{\dots}{arguments passed to \code{\link[car]{linearHypothesis}}.}
}

\details{
These tools should help to automate exchange rate regime classification.

The first building block is the function \code{fxpegtest}, a simple convenience
interface to \code{\link[car]{linearHypothesis}}. It assess the null hypothesis
that only the \code{peg} currency has coefficient \code{1} and all other 
currencies have coefficient \code{0}.
}

\references{
  Shah A., Zeileis A., Patnaik I. (2005), What is the New Chinese
  Currency Regime?, Report 23, Department of Statistics and Mathematics,
  Wirtschaftsuniversitaet Wien, Research Report Series, November 2005.
  \url{http://epub.wu.ac.at}.

  Zeileis A., Shah A., Patnaik I. (2010), Testing, Monitoring, and Dating Structural
  Changes in Exchange Rate Regimes, \emph{Computational Statistics and Data Analysis},
  54(6), 1696--1706. \url{http://dx.doi.org/10.1016/j.csda.2009.12.005}.
}

\seealso{\code{\link[fxregime]{fxlm}}, \code{\link[fxregime]{fxregimes}}}

\examples{
## load package and data
library("fxregime")
data("FXRatesCHF", package = "fxregime")

## compute returns for CNY (and explanatory currencies)
## after abolishing fixed USD regime until end of 2005
cny <- fxreturns("CNY", frequency = "daily",
  start = as.Date("2005-07-25"), end = as.Date("2005-12-31"),
  other = c("USD", "JPY", "EUR", "GBP"))

## estimate full-sample exchange rate regression model
fm <- fxlm(CNY ~ USD + JPY + EUR + GBP, data = cny)

## check for plain USD peg:
fxpegtest(fm)
## no deviation from a plain USD peg
}

\keyword{regression}
