
#' Create a SpotRate object.
#' 
#' Creates a spot rate that is an interest rate related to a specific term.
#' It can be interpreted as the interest amount asked to for investments 
#' maturing at the term.
#' 
#' A SpotRate is composed by the value of its interest rate, the term, the 
#' amount of days in basis (days within a year) and the compounding regime
#' on which the rate is compounded.
#' The SpotRate can be compounded to generate a CompoundFactor, and that 
#' can be used in many calculations, for example, computing equivalent rates.
#' 
#' @param value the value of the underlying interest rate
#' @param term the term related to the interest paid
#' @param dib days in base (the number of days within a year)
#' @param compounding the compounding regime can assume the following values:
#' \code{simple}, \code{compounded} and \code{continuous}
#' @return a SpotRate object
#' @export
SpotRate <- function(value, term, dib=252, compounding='compounded') {
    that <- list()
    that$value <- value
    that$term <- term
    that$dib <- dib
    that$compounding <- compounding
    class(that) <- 'SpotRate'
    return(that)
}

#' is.SpotRate
#' 
#' Checks if is a SpotRate
#' 
#' @export
is.SpotRate <- function(object) class(object) == 'SpotRate'

#' as.SpotRate
#' 
#' Coerces to a SpotRate
#' 
#' @export
as.SpotRate <- function(object, ...) UseMethod('as.SpotRate', object)

#' @rdname as.SpotRate
#' @method as.SpotRate SpotRate
#' @S3method as.SpotRate SpotRate
as.SpotRate.SpotRate <- function(object, term=object$term) {
    SpotRate(value=object$value, term=term, dib=object$dib)
}

#' @rdname as.SpotRate
#' @method as.SpotRate CompoundFactor
#' @S3method as.SpotRate CompoundFactor
as.SpotRate.CompoundFactor <- function(object, dib=252, compounding='compounded') {
    rate <- attr(Compounding[[compounding]], 'implied.rate')(object$value, object$term, dib)
    SpotRate(rate, object$term, dib=dib, compounding=compounding)
}

#' @S3method all.equal SpotRate
all.equal.SpotRate <- function(target, current, tolerance=.Machine$double.eps^0.5, ...) {
    target$term == current$term &&
        target$dib == current$dib &&
        abs(target$value - current$value) <= tolerance
}

#' @S3method print SpotRate
print.SpotRate <- function(object) {
    cat('\nrate =', object$value, '\nterm =', object$term,
        '\ndays in base =', object$dib, '\n')
}

