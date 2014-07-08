\name{as.compounding}
\alias{as.compounding}
\alias{as.compounding.character}
\title{Create a compounding object}
\usage{
as.compounding(obj, ...)

\method{as.compounding}{character}(obj = c("discrete", "simple",
  "continuous"), ...)
}
\arguments{
  \item{obj}{one of those strings: \code{discrete},
  \code{simple}, \code{continuous}}

  \item{...}{extra arguments}
}
\value{
a compounding object
}
\description{
Compounding object creation function.
}
\examples{
as.compounding('simple')
}

