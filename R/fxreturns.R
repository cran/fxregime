## extract returns needed from a price series (FXRatesCHF, by default)
## either at weekly or daily frequency
fxreturns <- function(x, other = c("USD", "JPY", "DUR", "GBP"), data = NULL,
  frequency = "weekly", start = NULL, end = NULL, na.action = na.locf, trim = FALSE)
{
  if(is.null(data)) data <- fxregime::FXRatesCHF

  ## select columns x and other
  if(is.character(x))         x <- which(colnames(data) %in% x)
  if(is.character(other)) other <- which(colnames(data) %in% other)
  rval <- data[,unique(c(x, other))]
  
  ## select appropriate window
  if(!(is.null(start) & is.null(end))) rval <- window(rval, start = start, end = end)
  
  ## keep daily or aggregate to weekly series (anchored on Fridays)
  freq <- match.arg(frequency, c("weekly", "daily"))
  if(freq == "weekly") {
    ## convenience functions
    nextfri <- function(date) 7 * ceiling(as.numeric(date - 1)/7) + as.Date(1)
    mytail <- function(x) x[length(x)]
    ## aggregation (with special handling of NAs)
    rval <- aggregate(rval, nextfri, function(z) if(all(is.na(z))) NA else mytail(z[!is.na(z)]))
  }

  ## handle NAs
  rval <- na.action(rval)

  ## compute returns
  rval <- 100 * diff(log(rval))

  ## trimming
  if(is.null(trim)) trim <- FALSE
  if(!identical(trim, FALSE)) {
    if(isTRUE(trim)) trim <- c(0.01, 0.99)
    x <- coredata(rval)[,1]
    xq <- quantile(x, rep(sort(trim), length.out = 2))
    wi <- which(x < xq[1] | x > xq[2])
    rval <- rval[-wi,]
  }

  return(rval)
}
