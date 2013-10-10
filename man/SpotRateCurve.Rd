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
  Linear, FlatForward, LogLinear, Hermite Cubic Spline,
  Monotone Cubic Spline, Natural Cubi Spline}
}
\description{
  SpotRateCurve abstracts a term structure class providing
  methods to operate on the term structure internals
  executing calculations on its interest rates. Actually,
  the class SpotRateCurve represents a zero curve and for a
  given interpolation method (\code{interp} parameter),
  users can get zero interest rates for any day into the
  range deimited by its terms.
}
\examples{
# Creating a zero curve
curve <- SpotRateCurve(c(0.08, 0.083, 0.089, 0.093, 0.095), c(0.5, 1, 1.5, 2, 2.5))
# Creating a zero curve with linear interpolation and 360 days in base (days per year)
curve <- SpotRateCurve(c(0.08, 0.083, 0.089, 0.093, 0.095), c(0.5, 1, 1.5, 2, 2.5), interp='Linear', dib=360)
}

