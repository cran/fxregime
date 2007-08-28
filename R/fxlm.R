## Frankel-Wei linear regression (simple wrapper for lm())
fxlm <- function(formula, data, ...) {
  cl <- match.call()
  if(missing(formula)) formula <- colnames(data)[1]
  if(is.character(formula)) formula <- as.formula(paste(formula,
    "~", paste(colnames(data)[colnames(data) != formula], collapse = " + ")))
  rval <- lm(formula, data = as.data.frame(data), ...)
  rval$call <- cl
  rval$index <- index(data)
  class(rval) <- c("fxlm", "lm")
  return(rval)
}

## and corresponding coefficients and estimating functions
coef.fxlm <- function(object, ...) {
  rval <- NextMethod(object)
  rval <- c(rval, "(Variance)" = mean(residuals(object)^2))
  return(rval)
}

estfun.fxlm <- function(x, ...) {
  res <- residuals(x)
  ef <- NextMethod(x)
  sigma2 <- mean(res^2)
  rval <- cbind(ef, res^2 - sigma2)
  colnames(rval) <- c(colnames(ef), "(Variance)")
  if(!inherits(rval, "zoo"))
    rval <- zoo(rval, index(x))
  return(rval)
}

## index/time extraction
time.fxlm <- index.fxlm <- function(x, ...) x$index

## test linear hypotheses
linear.hypothesis.fxlm <- function(model, ...) {
  stopifnot(require("car"))
  class(model) <- class(model)[-1]
  NextMethod()
}
