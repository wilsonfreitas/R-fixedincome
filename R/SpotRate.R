
#'
#' SpotRate - it is the pair (InterestRate, term)
#' A SpotRateCurve is a sorted set of SpotRates
#'

SpotRate <- function(value, term, dib=252, compounding='compounded') {
    that <- list()
    that$value <- value
    that$term <- term
    that$dib <- dib
    that$compounding <- compounding
    class(that) <- 'SpotRate'
    return(that)
}

#'
#' Methods assignments
#' 

rate <- function(object, ...) UseMethod('rate', object)
term <- function(object, ...) UseMethod('term', object)
dib <- function(object, ...) UseMethod('dib', object)
compounding <- function(object, ...) UseMethod('compounding', object)
forward.rate <- function(object, ...) UseMethod('forward.rate', object)
as.SpotRate <- function(object, ...) UseMethod('as.SpotRate', object)
as.CompoundFactor <- function(object, ...) UseMethod('as.CompoundFactor', object)

compound.factor <- function(object, ...) UseMethod('compound.factor', object)
compound <- function(object, ...) UseMethod('compound', object)
discount <- function(object, ...) UseMethod('discount', object)

#'
#' Methods implementation
#'

is.SpotRate <- function(object) class(object) == 'SpotRate'

rate.SpotRate <- function (object) object$value

term.SpotRate <- function (object) object$term

dib.SpotRate <- function (object) object$dib

compounding.SpotRate <- function (object) object$compounding

forward.rate.default <- function (object, ...) stop('No default implementation')

forward.rate.SpotRate <- function (object, other) {
    if (object$term > other$term)
        stop('First parameter must have the smaller term.')
    fact.1 <- as.CompoundFactor(object)
    fact.2 <- as.CompoundFactor(other)
    as.SpotRate(fact.2/fact.1)
}

as.SpotRate.default <- function(object, ...) stop('No default implementation')

as.SpotRate.SpotRate <- function(object, term=object$term) {
    SpotRate(value=object$value, term=term, dib=object$dib)
}

as.CompoundFactor.SpotRate <- function(object) {
    fact <- attr(Compounding[[object$compounding]], 'compound')(object$value, object$term, object$dib)
    return( CompoundFactor(fact, object$term) )
}

compound.factor.SpotRate <- function (object) {
    compound.factor(as.CompoundFactor(object))
}

all.equal.SpotRate <- function(target, current, tolerance=.Machine$double.eps^0.5, ...) {
    target$term == current$term &&
        target$dib == current$dib &&
        abs(target$value - current$value) <= tolerance
}

print.SpotRate <- function(object) {
    cat('\nrate =', object$value, '\nterm =', object$term,
        '\ndays in base =', object$dib, '\n')
}

