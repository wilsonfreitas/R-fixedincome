\name{[.SpotRateCurve}
\alias{[.SpotRateCurve}
\alias{[<-.SpotRateCurve}
\title{Insert a SpotRate into the SpotRate.}
\usage{
  \method{[}{SpotRateCurve} (object, term)

  \method{[}{SpotRateCurve} (object, i, value) <- value
}
\arguments{
  \item{object}{SpotRateCurve on which the SpotRate has to
  be inserted}

  \item{value}{SpotRate to be inserted}
}
\description{
  A SpotRateCurve can be expanded by inserting other
  SpotRate objects into it.
}
\examples{
# creating a SpotRateCurve from a matrix
mat <- cbind(c(0.08, 0.083, 0.089, 0.093, 0.095), c(0.5, 1, 1.5, 2, 2.5))
curve <- as.SpotRateCurve(mat, interp='Spline', dib=365)
curve[1.25] <- 0.085
}

