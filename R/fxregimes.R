## partition FX models based on normal loglik
## (wrapper to the currently unexported gbreakpoints)
fxregimes <- function(formula, data, ...) {
  if(missing(formula)) formula <- colnames(data)[1]
  if(is.character(formula)) formula <- as.formula(paste(formula,
    "~", paste(colnames(data)[colnames(data) != formula], collapse = " + ")))
  rval <- gbreakpoints(formula, data = data, order.by = time(data), ...)
  rval$formula <- formula
  rval$data <- data
  class(rval) <- c("fxregimes", class(rval))
  return(rval)
}

print.fxregimes <- function(x, digits = max(3, getOption("digits") - 3), ...) 
{
    cat("\nFX model: ", deparse(x$formula), "\n\n",
        paste("Minimum", x$icname, "partition\n"),
	"Number of regimes: ", length(x$breakpoints[!is.na(x$breakpoints)]) + 1, "\n",
	"Breakdates: ", paste(breakdates(x), collapse = ", "), "\n\n",
	sep = "")
    invisible(x)
}

index.fxregimes <- time.fxregimes <- function(x, ...) index(x$data)

lines.fxregimes <- function(x, breaks = NULL, ...)
  lines(breakdates(x, breaks = breaks), ...)


## obtain FX models for each segment
## (plus standard extractor functions)
refit.fxregimes <- function(object, breaks = NULL, ...) {
  if(is.null(breaks)) breaks <- time(object$data)[object$breakpoints]
    else if(length(breaks) == 1 & is.numeric(breaks) & !inherits(breaks, "Date"))
      breakdates(object, breaks = breaks)
  breaks <- breaks[!is.na(breaks)]
  
  if(length(breaks) < 1) {
    sbp <- start(object$data)
    ebp <- end(object$data)
  } else {  
    sbp <- c(start(object$data), sapply(breaks, function(z)
      index(object$data)[min(which(index(object$data) > z))]))
    ebp <- c(breaks, end(object$data))
  }
  
  rval <- lapply(1:length(sbp), function(i)
    fxlm(object$formula, data = window(object$data, start = sbp[i], end = ebp[i]), ...))
  names(rval) <- paste(as.character(sbp), as.character(ebp), sep = "--")
  return(rval)  
}

coef.fxregimes <- function(object, breaks = NULL, ...)
  t(sapply(refit(object, breaks = breaks, ...), coef))

fitted.fxregimes <- function(object, breaks = NULL, ...) {
  rval <- as.vector(unlist(lapply(refit(object, breaks = breaks, ...), fitted)))
  rval <- zoo(rval, index(object))
  return(rval)
}

residuals.fxregimes <- function(object, breaks = NULL, ...) {
  rval <- as.vector(unlist(lapply(refit(object, breaks = breaks, ...), residuals)))
  rval <- zoo(rval, index(object))
  return(rval)
}
