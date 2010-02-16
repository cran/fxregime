\name{fxreturns}
\alias{fxreturns}

\title{Compute Exchange Rate Returns}

\description{
Compute a multivariate series of exchange rate returns (log-returns in percent)
from a multivariate series of exchange rates.
}

\usage{
fxreturns(x, other = c("USD", "JPY", "DUR", "GBP"), data = FXRatesCHF,
  frequency = "weekly", start = NULL, end = NULL, na.action = na.locf, trim = FALSE)
}

\arguments{
  \item{x}{character with column name of selected target currency.}
  \item{other}{character vector with column names of further currencies (to be
    used as regressors).}
  \item{data}{a \code{"zoo"} time series with FX rates (with respect to a base currency).}
  \item{frequency}{character specifying whether weekly or daily returns should be computed.}
  \item{start}{start time of the exchange rate series (before computing returns).}
  \item{end}{end time of the exchange rate series.}
  \item{na.action}{function for handling \code{NA}s.}
  \item{trim}{logical or numeric. Should time points with extreme returns of the
    target currency be excluded? If set to \code{TRUE}, the quantiles \code{c(0.01, 0.99)}
    are used for trimming.}
}

\details{
\code{fxreturns} is a convenience function for selecting a smaller number
of currencies from a large database of exchange rates (such as \code{\link{FXRatesCHF}}
provided with the package), selecting a certain time window (if necessary),
computing returns (weekly or daily), eliminating missing values and potentially trimming
extreme returns (of the target currency).
}

\value{
A \code{"zoo"} with the returns of the specified currencies.
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

\seealso{\code{\link[fxregime]{fxlm}}, \code{\link[fxregime]{fxregimes}},
  \code{\link[fxregime]{fxmonitor}}}

\examples{
## load package and data
library("fxregime")
data("FXRatesCHF", package = "fxregime")

## compute returns for CNY (and explanatory currencies)
## for one year after abolishing fixed USD regime
cny <- fxreturns("CNY", frequency = "daily",
  start = as.Date("2005-07-25"), end = as.Date("2006-07-24"),
  other = c("USD", "JPY", "EUR", "GBP"))
plot(cny)
}

\keyword{regression}
