\name{compound.compounding}
\alias{compound}
\alias{compound.compounding}
\alias{compound.spotrate}
\alias{discount}
\alias{discount.spotrate}
\title{Compound function}
\usage{
\method{compound}{compounding}(obj, value, term, ...)

\method{compound}{spotrate}(obj, term, units = c("days", "months", "years"),
  from = NULL, to = NULL, ...)

\method{discount}{spotrate}(obj, term, units = c("days", "months", "years"),
  from = NULL, to = NULL, ...)

compound(obj, ...)

discount(obj, ...)
}
\arguments{
  \item{value}{a numeric representing spot rate value}

  \item{units}{a valid term unit: \code{"days"},
  \code{"months"}, \code{"years"}}

  \item{from}{a date object. Since it uses \code{bizdays}
  package it accepts many date formats.}

  \item{to}{a date object. Since it uses \code{bizdays}
  package it accepts many date formats.}

  \item{obj}{See Details}

  \item{term}{See Details}

  \item{...}{extra arguments}
}
\value{
a numeric value
}
\description{
Computes the compounding factor
}
\details{
If \code{obj} is a \code{\link{compounding-class}} instance
the \code{value} and \code{term} must be numeric values
representing spot rate and compounding term, respectively.

If \code{obj} is a \code{\link{spotrate-class}} the
\code{term} can be a \code{\link{term-class}} or a numeric.
For numeric term the \code{units} argument should be used
or it defaults to \code{"days"}. When the \code{term}
argument is missing the arguments \code{from} and \code{to}
must be provided and also the spot rate's \code{calendar}
attribute. The term is computed in days, the number of days
between the two dates according to \code{calendar}.

The \code{discount} is the inverse of \code{compound}
\deqn{discount = \frac{1}{compound}}
}

