\name{spotrate-class}
\alias{spotrate-class}
\title{The spotrate object}
\description{
The \code{spotrate} represents a spot rate and stores the
information needed to compound or discount that rate.
}
\details{
The \code{spotrate} class fully specifies spot rates. It
has the 4 attributes necessary to manipulate spot rates:
\itemize{ \item the spot rate values which are numeric
values representing the rate \item the compounding regime
that specifies how to compound the spot rate and is
specified by a \code{compounding} object \item the daycount
rule which computes the compounding periods right adjusted
to the spot rate frequency \item the calendar that is
responsible for computing days between 2 dates }

The \code{spotrate} class is a numeric vector with
attributes. Once it is a numeric vector it represents spot
rates sharing the same compounding, daycount and calendar
attributes. The coercion function \code{as.list.spotrate}
splits the vector into many single spot rate objects.
}
\note{
The \code{spotrate} class represents a annual spot rates.
That restriction might be removed in the future.
}

