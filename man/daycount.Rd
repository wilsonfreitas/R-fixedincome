\name{daycount}
\alias{as.daycount}
\alias{daycount}
\alias{daycount.spotrate}
\title{Day count convention class}
\usage{
daycount(object, ...)

as.daycount(daycount, calendar = NULL)

\method{daycount}{spotrate}(spr)
}
\description{
Day count convetion creation.
}
\details{
Day count convetion
}
\examples{
dc <- as.daycount('actual/360')
dib(dc)
timefactor(dc, 10, 'days')
}

