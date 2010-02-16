\name{FXRatesCHF}
\alias{FXRatesCHF}

\title{Exchange Rates with CHF Unit Currency}

\description{
Foreign exchange rates for 25 currencies with respect to
Swiss franks (CHF) from 1971-01-04 to 2010-02-12.
}

\usage{
  data("FXRatesCHF")
}

\format{
\code{FXRatesCHF} is a \code{"zoo"} series with \code{"Date"} time index
containing 25 daily time series from 1971-01-04 to 2010-02-12. The columns
correspond to the price currencies (in ISO 4217 format) with respect to CHF as
the unit currency.
}

\details{
Additionally, to the \code{DEM} and \code{EUR} series, it contains a column called
\code{DUR} containing the \code{EUR} rates starting from 1999 and the (converted)
\code{DEM} rates prior to 1999.
}

\source{
Available online from the US Federal Reserve:

\url{http://www.federalreserve.gov/releases/h10/Hist/}
}

\seealso{\code{\link[zoo]{zoo}}}

\examples{
data("FXRatesCHF")
## compute/visualize USD/CNY rate
cny_usd <- na.trim(FXRatesCHF[,"CNY"]/FXRatesCHF[,"USD"])
plot(cny_usd)
}

\keyword{datasets}
