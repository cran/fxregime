\name{confint.fxregimes}
\alias{confint.fxregimes}
\alias{print.confint.fxregimes}
\alias{breakddates.confint.fxregimes}
\alias{lines.confint.fxregimes}

\encoding{latin1}

\title{Confidence Intervals for Breaks Between Exchange Rate Regimes}

\description{
Confidence intervals for estimated changes/breaks between exchange rate regimes.
}

\usage{
\method{confint}{fxregimes}(object, parm = NULL, level = 0.95, breaks = NULL, meat. = NULL, \dots)
}

\arguments{
  \item{object}{An object of class \code{"fxregimes"} as fitted by \code{\link[fxregime]{fxregimes}}.}
  \item{parm}{integer. Either \code{parm} or \code{breaks} may be set, see below.}
  \item{level}{numeric. The confidence level to be used.}
  \item{breaks}{integer. The number of breaks to be extracted from \code{object} for
    which confidence intervals should be computed.}
  \item{meat.}{function. A function for extracting the meat of a sandwich estimator
    from a \code{\link[fxregime]{fxlm}} object. By default, the inverse of
    \code{\link[sandwich]{bread}} is used, i.e., a correctly specified model is assumed.}
  \item{\dots}{currently not used.}
}

\details{
  As the breakpoints are integers (observation numbers) the corresponding
  confidence intervals are also rounded to integers. The algorithm used
  is essentially the same as described for \code{\link[strucchange]{confint.breakpointsfull}}.
  The same distribution function is used, just the variance components are
  computed differently. Here, \code{\link[sandwich]{bread}} and 
  \code{\link[sandwich]{meat}} (or some of its HC/HAC counterparts) are
  used. See Zeileis, Shah, Patnaik (2008) for more details.
}

\value{
An object of class \code{"confint.fxregimes"}.
}

\references{
  Zeileis A., Kleiber C., Krämer W., Hornik K. (2003), Testing and Dating of
  Structural Changes in Practice, \emph{Computational Statistics and Data Analysis},
  \bold{44}, 109--123.

  Zeileis A., Shah A., Patnaik I. (2010), Testing, Monitoring, and Dating Structural
  Changes in Exchange Rate Regimes, \emph{Computational Statistics and Data Analysis},
  54(6), 1696--1706. \url{http://dx.doi.org/10.1016/j.csda.2009.12.005}.
}

\seealso{\code{\link[fxregime]{fxregimes}}, \code{\link[fxregime]{refit}},
  \code{\link[fxregime]{fxlm}}, \code{\link[strucchange]{confint.breakpointsfull}}}

\examples{
## load package and data
library("fxregime")
data("FXRatesCHF", package = "fxregime")

## compute returns for CNY (and explanatory currencies)
## for one year after abolishing fixed USD regime
cny <- fxreturns("CNY", frequency = "daily",
  start = as.Date("2005-07-25"), end = as.Date("2006-07-24"),
  other = c("USD", "JPY", "EUR", "GBP"))

## compute all segmented regression with minimal segment size of
## h = 20 and maximal number of breaks = 5.
reg <- fxregimes(CNY ~ USD + JPY + EUR + GBP,
  data = cny, h = 20, breaks = 5, ic = "BIC")
summary(reg)

## minimum BIC is attained for 2-segment (1-break) model
plot(reg)

## two regimes
## 1: tight USD peg
## 2: slightly more relaxed USD peg
round(coef(reg), digits = 3)
sqrt(coef(reg)[, "(Variance)"])

## inspect associated confidence intervals
ci <- confint(reg, level = 0.9)
ci
breakdates(ci)

## plot LM statistics along with confidence interval
fm <- fxlm(CNY ~ USD + JPY + EUR + GBP, data = cny)
scus <- gefp(fm, fit = NULL)
plot(scus, functional = supLM(0.1))
lines(ci)
}

\keyword{regression}
