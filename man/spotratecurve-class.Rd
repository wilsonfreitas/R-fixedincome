\name{spotratecurve-class}
\alias{spotratecurve-class}
\title{Creates a spotratecurve}
\arguments{
  \item{rates}{a vector with the interest rates}

  \item{terms}{a vector with the terms}

  \item{interp}{defines which interpolation method to use:
  Linear, FlatForward, LogLinear, Hermite Cubic Spline,
  Monotone Cubic Spline, Natural Cubi Spline}
}
\value{
spotratecurve object.
}
\description{
spotratecurve abstracts a term structure class providing
methods to operate on the term structure internals.
}
\details{
Actually, the class spotratecurve represents a zero curve.
A zero curve is a set of \code{rates} indexed by
\code{terms}. Each pair (rate,term) is a spot rate
(\code{\link{SpotRate}}) and it defines the cost of a loan
maturing in the term. Once a spotratecurve is created, any
rate can be retrieved by its term through the index
operator (\code{`[`}) which returns a
\code{\link{SpotRate}}. For those terms which don't have a
corresponding rate, the interpolation method is used to
generate a rate. The parameter \code{interp} specifies the
interpolation method to use. Since the operator \code{`[`}
returns a SpotRate the \code{dib} must be provided to
generate spot rates accordingly.
}
\examples{
# Creating a zero curve
curve <- spotratecurve(c(0.08, 0.083, 0.089, 0.093, 0.095),
c(0.5, 1, 1.5, 2, 2.5))
# Creating a zero curve with linear interpolation and 360 days in base
# (days per year)
curve <- spotratecurve(c(0.08, 0.083, 0.089, 0.093, 0.095),
c(0.5, 1, 1.5, 2, 2.5), dib=360, compounding='simple')
}

