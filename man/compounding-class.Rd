\name{compounding-class}
\alias{compounding-class}
\alias{continuousCompounding}
\alias{discreteCompounding}
\alias{simpleCompounding}
\title{The compounding object}
\usage{
discreteCompounding()

simpleCompounding()

continuousCompounding()
}
\description{
The \code{compounding} object abstracts the compounding
regime used to discount or compound a spot rate.
}
\details{
There are 3 compoundings: \itemize{ \item \code{simple} for
simple interest rate compounding \deqn{1 + rt} \item
\code{discrete} for compounded interest rate compounding
\deqn{(1 + r)^t} \item \code{continuous} for continuous
interest rate compounding \deqn{exp(rt)} }

The \code{compounding} can be directly instanciated using
one the functions: \code{simpleCompounding},
\code{discreteCompounding}, \code{continuousCompounding},
or through \code{\link{as.compounding}}.

The \code{compounding} class has 2 methods: \itemize{ \item
\code{compound} to compound a rate for a given term. \item
\code{rates} to compute the implied rate for a compound
factor in a given term. }
}
\examples{
simpleCompounding() # or as.compounding("simple")
discreteCompounding() # or as.compounding("discrete")
continuousCompounding() # or as.compounding("continuous")

comp <- as.compounding("discrete")
compound(comp, 0.06, 1) # equals (1 + 0.06)^1 = 1.06
rates(comp, 1.06, 1) # equals 0.06
}

