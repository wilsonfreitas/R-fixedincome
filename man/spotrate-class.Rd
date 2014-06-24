\name{spotrate-class}
\alias{as.spotrate}
\alias{as.spotrate.character}
\alias{as.spotrate.default}
\alias{calendar.spotrate}
\alias{compound.spotrate}
\alias{compounding.spotrate}
\alias{daycount.spotrate}
\alias{rates.spotrate}
\alias{spotrate-class}
\title{Create a spotrate spr.}
\usage{
as.spotrate(obj, ...)

\method{as.spotrate}{default}(obj, compounding, daycount, calendar = NULL,
  ...)

\method{as.spotrate}{character}(obj, ...)

\method{rates}{spotrate}(obj, ...)

\method{compounding}{spotrate}(obj, ...)

\method{daycount}{spotrate}(obj, ...)

\method{calendar}{spotrate}(obj, ...)

\method{compound}{spotrate}(obj, term, units = NULL, from = NULL,
  to = NULL, ...)
}
\arguments{
  \item{obj}{the value of the underlying interest rate}

  \item{term}{the term related to the interest paid}

  \item{daycount}{daycount instance}

  \item{calendar}{calendar instance}

  \item{compounding}{the compounding regime can assume the
  following values: \code{simple}, \code{compounded} and
  \code{continuous}}

  \item{units}{units}

  \item{from}{dates}

  \item{to}{dates}

  \item{...}{extra arguments}
}
\value{
a spotrate spr
}
\description{
Creates a spot rate that is an interest rate related to a
specific term. It can be interpreted as the interest amount
asked to for investments maturing at the term.
}
\details{
A spotrate is composed by the value of its interest rate,
the term, the amount of days in basis (days within a year)
and the compounding regime on which the rate is compounded.
The spotrate can be compounded to generate a
CompoundFactor, and that can be used in many calculations,
for example, computing equivalent rates.
}

