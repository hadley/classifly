\name{simvar}
\alias{simvar}
\title{Simulate observations from a vector...}
\usage{simvar(x, n=10, method="grid")}

\description{
  Simulate observations from a vector
}

\details{
  Given a vector of data this function will simulate data
  that could have come from that vector.

  There are three methods to choose from:

  \itemize{ \item nonaligned (default): grid + some random
  peturbation \item grid: grid of evenly spaced
  observations.  If a factor, all levels in a factor will
  be used, regardless of n \item random: a random uniform
  sample from the range of the variable }
}
\keyword{datagen}
\arguments{
  \item{x}{data vector}
  \item{n}{desired number of points (will not always be achieved)}
  \item{method}{grid simulation method.  See details.}
}
