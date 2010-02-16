\name{fxregimes}
\alias{fxregimes}
\alias{print.fxregimes}
\alias{index.fxregimes}
\alias{time.fxregimes}
\alias{lines.fxregimes}
\alias{coef.fxregimes}
\alias{fitted.fxregimes}
\alias{residuals.fxregimes}
\alias{refit.fxregimes}

\alias{gbreakpoints}
\alias{print.gbreakpoints}
\alias{plot.gbreakpointsfull}
\alias{summary.gbreakpoints}
\alias{logLik.gbreakpoints}
\alias{AIC.gbreakpoints}
\alias{confint.gbreakpoints}
\alias{breakdates.gbreakpoints}

\alias{summary.gbreakpointsfull}
\alias{print.summary.gbreakpointsfull}
\alias{plot.summary.gbreakpointsfull}
\alias{AIC.gbreakpointsfull}
\alias{breakpoints.gbreakpointsfull}

\encoding{latin1}

\title{Dating Breaks Between Exchange Rate Regimes}

\description{
Estimate changes/breaks between exchange rate regimes (based on exchange rate regression models).
}

\usage{
fxregimes(formula, data, \dots, hpc = c("none", "foreach"))
}

\arguments{
  \item{formula}{a \code{"formula"} describing the linear model to be fit (as
    in \code{\link{fxlm}}.}
  \item{data}{a \code{"zoo"} time series.}
  \item{\dots}{further arguments passed to \code{gbreakpoints} (currently unexported,
    see below), most of the arguments are identical to the \code{formula} method of
    \code{\link[strucchange]{breakpoints}}}
  \item{hpc}{a character specifying the high performance computing support.
    Default is \code{"none"}, can be set to \code{"foreach"}.}
}

\details{
\code{fxregimes} segments an exchange rate regression (Frankel-Wei regression)
into different regimes. The breakpoints are chosen to maximize the likelihood of
a Gaussian regression (with regime-specific variances). The number of breakpoints
are selected according to information criteria (by default LWZ, but BIC is also
reported).

The computing engine behind \code{fxregime} is \code{gbreakpoints} that generalizes
various aspects about \code{\link[strucchange]{breakpoints}}. Its interface
is not yet fixed, hence it is not exported in the namespace (and not documented).
Most arguments are similar to those of the \code{formula} method of \code{\link[strucchange]{breakpoints}},
in particular the minimal segment size \code{h} and the maximal number of breaks
\code{breaks}. To select the default information criterion the new argument
\code{ic} can be set either to \code{"LWZ"} (the default) or \code{"BIC"}.

A set of methods for useful generic functions is available, including \code{plot},
\code{print}, \code{summary}, \code{lines}, \code{coef}, \code{fitted}, \code{residuals},
\code{breakfactor}, \code{breakpoints}, \code{breakdates}, etc. Mostly, they behave
like their \code{"breakpoints"} counterparts. Querying information about the
models on each segment is made particularly easy by providing a \code{\link[fxregime]{refit}}
method that returns a list of \code{\link[fxregime]{fxlm}} objects.

Optional support for high performance computing is available, currently using
\code{\link[foreach]{foreach}} for the dynamic programming algorithm.
If \code{hpc = "foreach"} is to be used, a parallel backend should be registered
before. See \code{\link[foreach]{foreach}} for more information.
}

\value{
An object of class \code{"fxregimes"} inheriting from \code{"gbreakpointsfull"},
\code{"gbreakpoints"}, \code{"breakpointsfull"}, \code{"breakpoints"}.
}

\references{
  Zeileis A., Kleiber C., Krämer W., Hornik K. (2003), Testing and Dating of
  Structural Changes in Practice, \emph{Computational Statistics and Data Analysis},
  \bold{44}, 109-123.

  Shah A., Zeileis A., Patnaik I. (2005), What is the New Chinese
  Currency Regime?, Report 23, Department of Statistics and Mathematics,
  Wirtschaftsuniversitaet Wien, Research Report Series, November 2005.
  \url{http://epub.wu.ac.at}.

  Zeileis A., Shah A., Patnaik I. (2010), Testing, Monitoring, and Dating Structural
  Changes in Exchange Rate Regimes, \emph{Computational Statistics and Data Analysis},
  Forthcoming. \url{http://dx.doi.org/10.1016/j.csda.2009.12.005}.
}

\seealso{\code{\link[fxregime]{fxlm}}, \code{\link[fxregime]{fxregimes}},
  \code{\link[fxregime]{refit}}}

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

## inspect two individual models by re-fitting
refit(reg)
}

\keyword{regression}
