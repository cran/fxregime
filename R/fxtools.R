fxpegtest <- function(model, peg = NULL, ...) {
  stopifnot(require("car") & require("utils"))

  ## coefficients without intercept/variance
  cc <- coef(model)
  cc <- cc[-c(1, length(cc))]
  
  ## default peg: maximal coef
  if(is.null(peg)) peg <- names(cc)[which.max(abs(cc))]

  ## hypothesis: peg = 1, all other 0
  hyp <- rep(0, length(cc))
  names(hyp) <- names(cc)
  hyp[peg] <- 1
  hyp <- paste(names(hyp), hyp, sep = " = ")
  
  ## call linear.hypothesis()/linearHypothesis()
  class(model) <- "lm"
  rval <- if(compareVersion(packageDescription("car")$Version, "2.0-0") >= 0) {
    linearHypothesis(model, hyp, ...)
  } else {
    linear.hypothesis(model, hyp, ...)
  }

  ## output formatting
  ff <- as.character(formula(model))
  attr(rval, "heading") <- c("Wald test for pegged FX regime", "",
    paste("Model 1:", ff[2], ff[1], ff[3]),
    paste("Model 2:", ff[2], ff[1], peg, "(fixed = 1)"), "")

  return(rval)
}
