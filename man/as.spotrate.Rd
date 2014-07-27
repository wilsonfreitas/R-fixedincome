\name{as.spotrate}
\alias{as.spotrate}
\alias{as.spotrate.character}
\alias{as.spotrate.default}
\alias{as.spotrate.numeric}
\alias{as.spotrate.spotrate}
\title{Create a spotrate object}
\usage{
as.spotrate(obj, ...)

\method{as.spotrate}{default}(obj, compounding, daycount, calendar = NULL,
  ...)

\method{as.spotrate}{numeric}(obj, compounding, daycount, calendar = NULL,
  ...)

\method{as.spotrate}{character}(obj, ...)

\method{as.spotrate}{spotrate}(obj, term, compounding = NULL,
  daycount = NULL, calendar = NULL, ...)
}
\arguments{
  \item{obj}{numeric values defining rates or a string
  specifying a spot rate}

  \item{compounding}{an instance of
  \code{\link{compounding-class}}}

  \item{daycount}{an instance of
  \code{\link{daycount-class}}}

  \item{calendar}{and instance of bizdays'
  \code{\link[bizdays]{Calendar}}}

  \item{...}{extra arguments}
}
\value{
a spotrate object
}
\description{
A factory method of \code{spotrate} objects.
}
\details{
The \code{compounding} and \code{daycount} arguments are
necessary to create a spotrate. The \code{calendar}
argument is only necessary if someone wants to compound
spot rates between dates.

\code{as.spotrate.character} accepts a string in the format

\preformatted{"RATE COMPOUNDING DAYCOUNT"}

where: \itemize{ \item \code{RATE} is a numeric value \item
\code{COMPOUNDING} is one of the following strings
\code{simple}, \code{discrete}, \code{continuous} \item
\code{DAYCOUNT} is a valid day count rule, see
\code{\link{daycount-class}} }

That function returns a single \code{spotrate} (length 1).
If you have a character vector you should use the
\code{lapply} function to create a list of \code{spotrate}.

\preformatted{ specs <- c("0.06 simple actual/365", "0.11
discrete business/252") lapply(specs, as.spotrate) }
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

