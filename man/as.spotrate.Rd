\name{as.spotrate}
\alias{as.spotrate}
\alias{as.spotrate.character}
\alias{as.spotrate.default}
\title{Crete a spotrate object}
\usage{
as.spotrate(obj, ...)

\method{as.spotrate}{default}(obj, compounding, daycount, calendar = NULL,
  ...)

\method{as.spotrate}{character}(obj, ...)
}
\arguments{
  \item{obj}{a numeric value which defines a rate or a
  string specifying a spot rate.}

  \item{compounding}{the compounding regime can assume the
  following values: \code{simple}, \code{compounded} and
  \code{continuous}}

  \item{daycount}{daycount instance}

  \item{calendar}{calendar instance}

  \item{...}{unused extra arguments}
}
\value{
a spotrate object
}
\description{
A factory method of \code{spotrate} objects.
}
\details{
\code{as.spotrate.default} accepts any objects that coerce
to numeric. The \code{compounding} and \code{daycount}
arguments are necessary to create a spotrate. The
\code{calendar} is only necessary if someone wants to
compound spot rates providing dates (\code{from} and
\code{to}) instead of terms.

\code{as.spotrate.character} accepts a string in the format
\code{RATE COMPOUNDING DAYCOUNT}, where: \itemize{ \item
\code{RATE} a numeric value \item \code{COMPOUNDING} one of
the following: \code{simple}, \code{discrete},
\code{continuous} \item \code{DAYCOUNT} a valid day count
rule, see \code{\link{daycount-class}} } That function
returns a single spotrate (length 1). If you have a
character vector you should use the \code{lapply} function
to create a list of spotrates. \preformatted{ specs <-
c("0.06 simple actual/365", "0.11 discrete business/252")
lapply(specs, as.spotrate) }
}
\note{
The \code{spotrate} class represents a annual spot rates.
That restriction might be removed in the future.
}
\examples{
as.spotrate("0.06 simple actual/365")
library(bizdays)
as.spotrate(0.06, continuousCompounding(), as.daycount("actual/365"),
Calendar(name="actual"))
as.spotrate(c(0.06, 0.07, 0.08), simpleCompounding(),
as.daycount("actual/365"))
specs <- c("0.06 simple actual/365", "0.11 discrete business/252")
lapply(specs, as.spotrate)
}

