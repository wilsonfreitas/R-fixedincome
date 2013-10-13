\name{CompoundFactor}
\alias{CompoundFactor}
\title{Creates a CompoundFactor object related to a term.}
\usage{
  CompoundFactor(value, term)
}
\arguments{
  \item{value}{the factor's value}

  \item{term}{the compounding factor term}
}
\value{
  CompoundFactor instance
}
\description{
  CompoundFactor abstracts a compounding factor related to
  the given term. This CompoundFactor can discount future
  cash flows and compute future values of an investiment.
}
\details{
  The compounding factor has the same value for any
  compounding regime: simple, compounded, or continuous,
  and because of that it can be used to compute equivalent
  rates among different compoundings.

  \deqn{ CompoundFactor_{continuous} = \exp{i \cdot n} }
}

