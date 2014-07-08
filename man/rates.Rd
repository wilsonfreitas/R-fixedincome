\name{rates.compounding}
\alias{rates}
\alias{rates.compounding}
\alias{rates.spotrate}
\title{Return the numeric rates}
\usage{
\method{rates}{compounding}(obj, value, term, ...)

rates(obj, ...)

\method{rates}{spotrate}(obj, ...)
}
\arguments{
  \item{value}{a numeric value representing a compounding
  factor}

  \item{term}{a \code{\link{term-class}} instance}

  \item{obj}{See Details}

  \item{...}{extra arguments}
}
\value{
a numeric value
}
\description{
Return a numeric value which represents spot rates.
}
\details{
If the \code{obj} argument is a
\code{\link{compounding-class}} the function
\code{rates.compounding} computes the implied rate for the
given compounding and term.

If the \code{obj} argument is a \code{spotrate} instance it
returns a \code{numeric} representing the spot rates.
}

