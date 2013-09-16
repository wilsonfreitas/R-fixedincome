#'
#' CompoundFactor
#'

CompoundFactor <- function(value, term) {
    that <- list()
    that$value <- value
    that$term <- term
    
    class(that) <- 'CompoundFactor'
    return(that)
}

compound.factor <- function(object, ...) UseMethod('compound.factor', object)
term <- function(object, ...) UseMethod('term', object)
rate <- function(object, ...) UseMethod('rate', object)
as.SpotRate <- function(object, ...) UseMethod('as.SpotRate', object)
as.CompoundFactor <- function(object, ...) UseMethod('as.CompoundFactor', object)
compound <- function(object, ...) UseMethod('compound', object)
discount <- function(object, ...) UseMethod('discount', object)

compound.factor.CompoundFactor <- function(object) object$value

term.CompoundFactor <- function(object) object$term

as.SpotRate.CompoundFactor <- function(object, dib=252, compounding='compounded') {
    rate <- attr(Compounding[[compounding]], 'implied.rate')(object$value, object$term, dib)
    SpotRate(rate, object$term, dib=dib, compounding=compounding)
}

as.CompoundFactor.default <- function(object, ...) stop('No default implementation')

as.CompoundFactor.CompoundFactor <- function(object) {
    CompoundFactor(value=object$value, term=object$term)
}

rate.CompoundFactor <- function(object, dib=252, compounding='compounded') {
    rate(as.SpotRate(object, dib=dib, compounding=compounding))
}

compound.CompoundFactor <- function(cf.1, cf.2) {
    CompoundFactor(cf.1$value * cf.2$value, cf.1$term + cf.2$term)
}

discount.CompoundFactor <- function(cf.1, cf.2) {
    CompoundFactor(cf.2$value / cf.1$value, cf.2$term - cf.1$term)
}

'*.CompoundFactor' <- function(cf.1, cf.2) {
    compound(cf.1, cf.2)
}

'/.CompoundFactor' <- function(cf.2, cf.1) {
    CompoundFactor(cf.2$value/cf.1$value, cf.2$term - cf.1$term)
}

all.equal.CompoundFactor <- function(target, current, tolerance=.Machine$double.eps^0.5, ...) {
    target$term == current$term &&
        abs(target$value - current$value) <= tolerance
}

print.CompoundFactor <- function(object) {
    cat('\ncompounding factor =', object$value, '\nterm =', object$term, '\n')
}

