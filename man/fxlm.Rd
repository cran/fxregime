\name{fxlm}
\alias{fxlm}
\alias{coef.fxlm}
\alias{estfun.fxlm}
\alias{bread.fxlm}
\alias{time.fxlm}
\alias{index.fxlm}

\title{Exchange Rate Regression}

\description{
Interface to \code{\link[stats]{lm}} for fitting exchange rate regression
models (Frankel-Wei models).
}

\usage{
  fxlm(formula, data, \dots)
}

\arguments{
  \item{formula}{a \code{"formula"} describing the linear model to be fit.
    For details see below.}
  \item{data}{a \code{"zoo"} time series.}
  \item{\dots}{arguments passed to \code{\link[stats]{lm}}.}
}

\details{
\code{fxlm} is a function for fitting exchange rate regression models also
known as Frankel-Wei models. It is a simple convenience interface to \code{lm}:
\code{data} is assumed to be a \code{"zoo"} series in which, by default, the
first column is the dependent variable. If \code{formula} is omitted, the first
column is regressed on the remaining columns in \code{data}. The main difference
compared to plain \code{lm} models is that the error variance is reported as
a full parameter (estimated by maximum likelihood) in the \code{coef} method
and the \code{estfun} method (but currently not in the \code{\link[stats]{vcov}} method). 
Furthermore, the index (also known as the time stamps) of the underlying data set
can be extracted by the \code{time}/\code{index} method.
}

\value{
An object of class \code{"fxlm"} inheriting from \code{"lm"}.
}

\references{
  Shah A., Zeileis A., Patnaik I. (2005), What is the New Chinese
  Currency Regime?, Report 23, Department of Statistics and Mathematics,
  Wirtschaftsuniversitaet Wien, Research Report Series, November 2005.
  \url{http://epub.wu.ac.at}.

  Zeileis A., Shah A., Patnaik I. (2010), Testing, Monitoring, and Dating Structural
  Changes in Exchange Rate Regimes, \emph{Computational Statistics and Data Analysis},
  Forthcoming. \url{http://dx.doi.org/10.1016/j.csda.2009.12.005}.
}

\seealso{\code{\link[stats]{lm}}, \code{\link[fxregime]{fxregimes}}}

\examples{
## load package and data
library("fxregime")
data("FXRatesCHF", package = "fxregime")

## compute returns for CNY (and explanatory currencies)
## for one year after abolishing fixed USD regime
cny <- fxreturns("CNY", frequency = "daily",
  start = as.Date("2005-07-25"), end = as.Date("2006-07-24"),
  other = c("USD", "JPY", "EUR", "GBP"))

## estimate full-sample exchange rate regression
fm <- fxlm(CNY ~ USD + JPY + EUR + GBP, data = cny)
coef(fm)
summary(fm)

## test parameter stability (with double max test)
scus <- gefp(fm, fit = NULL)
plot(scus, aggregate = FALSE)
## which shows a clear increase in the variance in March 2006

## alternative tests: Andrews' supLM ...
plot(scus, functional = supLM(0.1))
## ... or Nyblom-Hansen test (Cramer-von Mises type test)
plot(scus, functional = meanL2BB)
}

\keyword{regression}
