\name{spotrate}
\alias{spotrate}
\title{Create a spotrate spr.}
\usage{
spotrate(value, compounding, daycount, calendar = NULL)
}
\arguments{
  \item{value}{the value of the underlying interest rate}

  \item{term}{the term related to the interest paid}

  \item{dib}{days in base (the number of days within a
  year)}

  \item{compounding}{the compounding regime can assume the
  following values: \code{simple}, \code{compounded} and
  \code{continuous}}
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

