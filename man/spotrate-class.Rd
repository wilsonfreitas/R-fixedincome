\name{spotrate-class}
\alias{spotrate-class}
\title{The spotrate object}
\description{
The \code{spotrate} object abstracts a spot rate (or an
interst rate) and stores all information needed to compound
or discount that rate.
}
\details{
The \code{spotrate} class fully specifies spot rates. It
has: \itemize{ \item the spot rate values which are numeric
values representing the rate. \item the compounding regime
that specifies how to compound the spot rate. This is a
\code{compounding} object. \item the daycount rule to
compute the compounding periods right adjusted to the spot
rate frequency (which is annual). \item the calendar which
returns the number of days between 2 dates. }

The \code{spotrate} class is a \code{numeric} vector and
all values in a \code{spotrate} instance share the
\code{compounding}, \code{daycount} and \code{calendar}
attributes. The coercion function \code{as.list.spotrate}
splits the vector into many single spot rate objects.

The \code{calendar} attribute is an instance of
\code{bizdays} \code{\link[bizdays]{Calendar}} class.
}
\note{
The \code{spotrate} objects are annual spot rates. That
restriction might be removed in the future.
}
\examples{
as.spotrate("0.06 simple actual/365")
library(bizdays)
as.spotrate(0.06, as.compounding('continuous'), as.daycount("actual/365"),
Calendar(name="actual"))
as.spotrate(c(0.06, 0.07, 0.08), as.compounding('simple'),
as.daycount("actual/365"))
specs <- c("0.06 simple actual/365", "0.11 discrete business/252")
lapply(specs, as.spotrate)
}

