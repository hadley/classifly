% Generated by roxygen2 (4.0.0): do not edit by hand
\name{advantage}
\alias{advantage}
\title{Calculate the advantage the most likely class has over the next most
likely.}
\usage{
advantage(post)
}
\arguments{
\item{post}{matrix of posterior probabilities}
}
\description{
This is used to identify the boundaries between classification regions.
Points with low (close to 0) advantage are likely to be near boundaries.
}
\keyword{classif}
