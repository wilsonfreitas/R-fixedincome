\name{as.term}
\alias{as.term}
\alias{as.term.character}
\alias{as.term.numeric}
\alias{as.term.term}
\title{Generic term class}
\usage{
as.term(object, ...)

\method{as.term}{numeric}(term, units)

\method{as.term}{character}(term, units = NULL)

\method{as.term}{term}(term, units = NULL)
}
\arguments{
  \item{term}{the term definition can be character, numeric
  or term. numeric must provide the units argument.
  character is a string which concatenates a numeric and
  one of the following: days, months, years. term is to
  convert between months and years units. Once the units
  arguments is provided for character it is also converted
  to the given units.}

  \item{units}{the units intended to be returned.}
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
#
}

