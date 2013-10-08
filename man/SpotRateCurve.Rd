\name{SpotRateCurve}
\alias{SpotRateCurve}
\title{Creates a SpotRateCurve}
\usage{
  SpotRateCurve(rates, terms, interp = "FlatForward",
    dib = 252)
}
\arguments{
  \item{rates}{a vector with the interest rates}

  \item{terms}{a vector with the terms}

  \item{interp}{defines which interpolation method to use:
  Linear, FlatForward, LogLinear,}
}
\description{
  SpotRateCurve abstracts a term structure class providing
  methods to operate on the term structure.
}
\examples{
# holidays has iso-formated dates
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
# For empty calendar just pass nothing
cal <- Calendar()
}

