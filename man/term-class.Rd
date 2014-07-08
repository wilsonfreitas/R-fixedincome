\name{term-class}
\alias{as.term}
\alias{as.term.character}
\alias{as.term.numeric}
\alias{as.term.term}
\alias{term-class}
\title{Generic term class}
\usage{
as.term(obj, ...)

\method{as.term}{numeric}(obj, units, ...)

\method{as.term}{character}(obj, units = NULL, ...)

\method{as.term}{term}(obj, units = NULL, ...)
}
\arguments{
  \item{obj}{can be another \code{term} instance, a string
  specifing a \code{term} or a number, and if the last a
  \code{units} must be provided.}

  \item{units}{one of the valid \code{units}: \code{days},
  \code{monts}, \code{years}.}

  \item{...}{extra arguments}
}
\description{
Terms creation, printing, and some arithmetic.
}
\details{
Terms are time intervals in days, months or years, which
are appropriate for fixed income calculations. The term
class represents the period used to discount or compound a
spot rate. That period represented by the term can be in
days, years or months and that is the term units.

Conversions are allowed from months to years and vice
versa. Conversions involving days cannot be done since the
number of days for months and years varies from one day
count convetion to another.

The term class inherits from the difftime class (base
package).
}
\examples{
as.term(6, 'months')
as.term('6 months')
as.term(as.term('6 months'), units='years')
as.numeric(as.term('6 months'))
}

