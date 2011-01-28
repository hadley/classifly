\name{advantage}
\alias{advantage}
\title{Calculate the advantage the most likely class has over the next most...}
\usage{advantage(post)}

\description{
  Calculate the advantage the most likely class has over
  the next most likely.
}

\details{
  This is used to identify the boundaries between
  classification regions. Points with low (close to 0)
  advantage are likely to be near boundaries.
}
\keyword{classif}
\arguments{
  \item{post}{matrix of posterior probabilities}
}
