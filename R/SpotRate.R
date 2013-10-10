
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

#' @export
rate <- function(object, ...) UseMethod('rate', object)

#' @export
term <- function(object, ...) UseMethod('term', object)

#' @export
dib <- function(object, ...) UseMethod('dib', object)

#' @export
compounding <- function(object, ...) UseMethod('compounding', object)

#' @export
forward.rate <- function(object, ...) UseMethod('forward.rate', object)

#' @export
as.SpotRate <- function(object, ...) UseMethod('as.SpotRate', object)

#' @export
as.CompoundFactor <- function(object, ...) UseMethod('as.CompoundFactor', object)

#' @export
compound.factor <- function(object, ...) UseMethod('compound.factor', object)

#' @export
compound <- function(object, ...) UseMethod('compound', object)

#' @export
discount <- function(object, ...) UseMethod('discount', object)

#' @export
is.SpotRate <- function(object) class(object) == 'SpotRate'

#' @export
rate.SpotRate <- function (object) object$value

#' @export
term.SpotRate <- function (object) object$term

#' @export
dib.SpotRate <- function (object) object$dib

#' @export
compounding.SpotRate <- function (object) object$compounding

#' @export
forward.rate.default <- function (object, ...) stop('No default implementation')

#' @export
forward.rate.SpotRate <- function (object, other) {
    if (object$term > other$term)
        stop('First parameter must have the smaller term.')
    fact.1 <- as.CompoundFactor(object)
    fact.2 <- as.CompoundFactor(other)
    as.SpotRate(fact.2/fact.1)
}

#' @export
as.SpotRate.default <- function(object, ...) stop('No default implementation')

#' @export
as.SpotRate.SpotRate <- function(object, term=object$term) {
    SpotRate(value=object$value, term=term, dib=object$dib)
}

#' @export
as.CompoundFactor.SpotRate <- function(object) {
    fact <- attr(Compounding[[object$compounding]], 'compound')(object$value, object$term, object$dib)
    return( CompoundFactor(fact, object$term) )
}

#' @export
compound.factor.SpotRate <- function (object) {
    compound.factor(as.CompoundFactor(object))
}

#' @export
all.equal.SpotRate <- function(target, current, tolerance=.Machine$double.eps^0.5, ...) {
    target$term == current$term &&
        target$dib == current$dib &&
        abs(target$value - current$value) <= tolerance
}

#' @export
print.SpotRate <- function(object) {
    cat('\nrate =', object$value, '\nterm =', object$term,
        '\ndays in base =', object$dib, '\n')
}

