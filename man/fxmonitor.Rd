\name{fxmonitor}
\alias{fxmonitor}
\alias{plot.fxmonitor}
\alias{print.fxmonitor}
\alias{breakpoints.fxmonitor}
\alias{breakdates.fxmonitor}

\title{Monitor Exchange Rate Regressions}

\description{
Score-based monitoring of exchange rate regression models (Frankel-Wei models).
}

\usage{
fxmonitor(formula, data, start, end = 3, alpha = 0.05, meat. = NULL)

\method{plot}{fxmonitor}(x, which = NULL, aggregate = NULL,
  ylim = NULL, xlab = "Time", ylab = "Empirical fluctuation process",
  main = "Monitoring of FX model", \dots)
}

\arguments{
  \item{formula}{a \code{"formula"} describing the linear model to be fit (as
    in \code{\link{fxlm}}.}
  \item{data}{a \code{"zoo"} time series (including history and monitoring time period).}
  \item{start}{starting time (typically in \code{"Date"} format) of the monitoring period.}
  \item{end}{end of the monitoring period (in scaled time, i.e., total length divided
    by length of history period).}
  \item{alpha}{significance level of the monitoring procedure.}
  \item{meat.}{optionally the meat of an alternative covariance matrix.}
  \item{x}{an object of class \code{"fxmonitor"} as fitted by \code{fxmonitor}.}
  \item{which}{name or number of parameter/process to plot.}
  \item{aggregate}{logical. Should the multivariate monitoring process be aggregated
    (using the absolute maximum)? Default is to aggregate for multivariate series.}
  \item{ylim, xlab, ylab, main, ...}{graphical parameters.}
}

\details{
\code{fxmonitor} is a function for monitoring exchange rate regression models (also
known as Frankel-Wei models). It fits the model on the history period (before \code{start})
and computes the predicted scores (or estimating functions) on the monitoring period.
The scaled and decorrelated process can be employed for monitoring as described by
Zeileis (2005) using a double-maximum-type procedure with linear boundary. The
critical values are interpolated from Table III in Zeileis et al. (2005).

Because the model just has to be fitted once (and not updated with every incoming
observation), the interface of \code{fxmonitor} is much simpler than that
of \code{\link[strucchange]{mefp}}: The \code{data} should just include all
available observations (including history and monitoring period). Instead of
updating the model with each incoming observation, the whole procedure has to be
repeated.

The \code{plot} method visualizes the monitoring process along with its boundaries.
The \code{print} method reports the breakdate (time of the first boundary crossing, if any),
which can also be queried by the \code{breakpoints} and \code{breakdates} methods.
}

\value{
An object of class \code{"fxmonitor"} which is a list including components:
  \item{process}{the fitted empirical fluctuation process,}
  \item{n}{the number of observations in the history period,}
  \item{formula}{the formula used,}
  \item{data}{the data used,}
  \item{monitor}{start of the monitoring period,}
  \item{end}{end of monitoring period,}
  \item{alpha}{significance level of monitoring procedure,}
  \item{critval}{the critical value (for a linear boundary).}
}

\references{
  Zeileis A., Leisch F., Kleiber C., Hornik K. (2005), Monitoring
  Structural Change in Dynamic Econometric Models,
  \emph{Journal of Applied Econometrics}, \bold{20}, 99--121.

  Zeileis A. (2005), A Unified Approach to Structural Change Tests Based on
  ML Scores, F Statistics, and OLS Residuals. \emph{Econometric Reviews}, \bold{24},
  445--466.

  Shah A., Zeileis A., Patnaik I. (2005), What is the New Chinese
  Currency Regime?, Report 23, Department of Statistics and Mathematics,
  Wirtschaftsuniversitaet Wien, Research Report Series, November 2005.
  \url{http://epub.wu.ac.at}.

  Zeileis A., Shah A., Patnaik I. (2010), Testing, Monitoring, and Dating Structural
  Changes in Exchange Rate Regimes, \emph{Computational Statistics and Data Analysis},
  Forthcoming. \url{http://dx.doi.org/10.1016/j.csda.2009.12.005}.
}

\seealso{\code{\link[fxregime]{fxlm}}, \code{\link[fxregime]{fxregimes}}}

\examples{
## load package and data
library("fxregime")
data("FXRatesCHF", package = "fxregime")

## compute returns for CNY (and explanatory currencies)
## for one year after abolishing fixed USD regime
cny <- fxreturns("CNY", frequency = "daily",
  start = as.Date("2005-07-25"), end = as.Date("2006-07-24"),
  other = c("USD", "JPY", "EUR", "GBP"))

## monitor CNY regression as in Shah et al. (2005)
mon <- fxmonitor(CNY ~ USD + JPY + EUR + GBP,
  data = cny, start = as.Date("2005-11-01"))
mon

## visualization
plot(mon)
plot(mon, aggregate = FALSE)
plot(mon, which = "(Variance)")

## query breakpoint/date
breakpoints(mon)
breakdates(mon)
}

\keyword{regression}
