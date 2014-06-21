\name{daycount-class}
\alias{as.daycount}
\alias{as.term.daycount}
\alias{daycount-class}
\alias{dib.daycount}
\alias{timefactor.daycount}
\title{Day count convention class}
\usage{
as.daycount(dcspec)

\method{dib}{daycount}(daycount)

\method{timefactor}{daycount}(daycount, term, units = NULL)

\method{as.term}{daycount}(daycount, term, units)
}
\arguments{
  \item{daycount}{an instance of \code{daycount}}

  \item{dcspec}{a string defining the day count convention:
  \code{30/360}, \code{30E/360}, \code{actual/365},
  \code{actual/360}, \code{business/252}}

  \item{term}{a valid term}

  \item{units}{a valid units (\code{days}, \code{months},
  \code{years})}
}
\description{
\code{daycount} class computes terms used to discount and
compound interest rates.
}
\details{
The day count convention determines the amount of days, in
years, for a given term.

\code{as.daycount} creates a \code{daycount} object and
accepts the following options: \code{30/360},
\code{30E/360}, \code{actual/365}, \code{actual/360},
\code{business/252}

\code{dib} returns the days in base for a day count
convention. Since we work with annual rates the days in
base define the amount of days in a year. Given that rule
the amount of days in a month is obtained by the fraction
of 1/12 of days in base \code{dib}.

\code{timefactor} returns the given term in years, since we
are assuming we work with annual rates. The \code{term}
argument can be a term instance, a string defining a term
or a number, but in the last a \code{units} must be
provided.

\code{as.term} is a convertion function, it converts terms
units. Since we don't have the amount of days per year,
while creating a term we can't make conversions involving
days. That's the reason why that function exists, to
convert using days. The \code{term} argument must be a
valid term or a valid string defining a term. The argument
\code{units} is the resulting units.
}
\examples{
dc <- as.daycount('actual/360')
dib(dc)
timefactor(dc, 10, 'days')
as.term(dc, '10 days', 'months')
}

