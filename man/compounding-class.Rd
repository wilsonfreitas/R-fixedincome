\name{compounding-class}
\alias{as.compounding}
\alias{as.compounding.character}
\alias{compound.compounding}
\alias{compounding-class}
\alias{continuousCompounding}
\alias{discreteCompounding}
\alias{rates.compounding}
\alias{simpleCompounding}
\title{Compounding class}
\usage{
as.compounding(obj, ...)

\method{as.compounding}{character}(obj = c("discrete", "simple",
  "continuous"), ...)

discreteCompounding()

simpleCompounding()

continuousCompounding()

\method{compound}{compounding}(obj, value, term, ...)

\method{rates}{compounding}(obj, value, term, ...)
}
\arguments{
  \item{obj}{an instance of compounding class}

  \item{value}{a numeric representing a compounding factor
  or a interest rate}

  \item{term}{a valid term}

  \item{...}{extra arguments}
}
\description{
Compounding
}
\details{
Compounding class
}

