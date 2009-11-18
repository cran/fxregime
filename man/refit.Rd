\name{refit}
\alias{refit}

\title{Refitting Models}

\description{
Generic function for refitting a models on various subsets or
reweighted data sets.
}

\usage{
  refit(object, \dots)
}

\arguments{
  \item{object}{an object.}
  \item{\dots}{arguments passed to methods.}
}

\details{
  \code{refit} is a new generic function for refitting a certain
  model object on multiple versions of a data set (and is hence
  different from \code{update}). Applications are partitioned/segmented
  models where the same type of model should be refitted on each
  sub-sample or partition. See \code{\link[fxregime]{fxregimes}}
  for such an example. Another application would be reweighted
  models such as in mixture models.
    
  The generic is similar to the one provided in \pkg{modeltools}
  (and should fulfill the same purpose). To avoid dependence on
  this package, it is also provided here.
}

\seealso{\code{\link[fxregime]{fxregimes}}}

\keyword{regression}
