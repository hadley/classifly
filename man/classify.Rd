\name{classify}
\alias{classify}
\title{Extract classifications from a variety of methods.}
\usage{classify(model, data, ...)}

\description{
  Extract classifications from a variety of methods.
}

\details{
  If the classification method can produce a matrix of
  posterior probabilities (see \code{\link{posterior}}),
  then that will be used to calculate the
  \code{\link{advantage}}.  Otherwise, the classify method
  will be used and the advantage calculated using a
  k-nearest neighbours approach.
}
\keyword{internal}
\arguments{
  \item{model}{model object}
  \item{data}{data set used in model}
  \item{...}{other argument passed on to methods}
}
