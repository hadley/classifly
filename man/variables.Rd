\name{variables}
\alias{variables}
\title{Extract predictor and response variables for a model object.}
\usage{variables(model)}

\description{
  Extract predictor and response variables for a model
  object.
}

\details{
  Due to the way that most model objects are stored, you
  also need to supply the data set you used with the
  original data set.  It currently doesn't support models
  fitted without using a data argument.
}
\value{list containing response and predictor variables}
\keyword{attribute}
\arguments{
  \item{model}{model object}
}
