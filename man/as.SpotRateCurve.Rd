\name{as.SpotRateCurve}
\alias{as.SpotRateCurve}
\alias{as.SpotRateCurve.data.frame}
\alias{as.SpotRateCurve.matrix}
\title{Coerce objects to SpotRateCurve}
\usage{
  as.SpotRateCurve(object, ...)

  \method{as.SpotRateCurve}{data.frame} (object, ...)

  \method{as.SpotRateCurve}{data.frame} (object, ...)
}
\arguments{
  \item{object}{should be a \code{matrix} or a
  \code{data.frame}}

  \item{...}{other SpotRateCurve's parameters}
}
\value{
  SpotRateCurve object

  SpotRateCurve object

  SpotRateCurve object
}
\description{
  A SpotRateCurve can be coerced from \code{matrix} and
  \code{data.frame}.
}
\examples{
# creating a SpotRateCurve from a data.frame
df <- data.frame(rates=c(0.08, 0.083, 0.089, 0.093, 0.095), terms=c(0.5, 1, 1.5, 2, 2.5))
curve <- as.SpotRateCurve(df, interp='Linear', dib=360)
# creating a SpotRateCurve from a matrix
mat <- cbind(c(0.08, 0.083, 0.089, 0.093, 0.095), c(0.5, 1, 1.5, 2, 2.5))
curve <- as.SpotRateCurve(mat, interp='Spline', dib=365)
}

