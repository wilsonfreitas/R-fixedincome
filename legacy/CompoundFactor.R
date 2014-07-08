#' @title
#' Creates a CompoundFactor object related to a term.
#' 
#' @description
#' CompoundFactor abstracts a compounding factor related to the given term.
#' This CompoundFactor can discount future cash flows and compute future values
#' of an investiment.
#' 
#' @details
#' The compounding factor has the same value for any compounding regime: simple,
#' compounded, or continuous, and because of that it can be used to compute 
#' equivalent rates among different compoundings.
#' 
#' \deqn{
#' CompoundFactor_{continuous} = \exp{i \cdot n}
#' }
#' 
#' @param value the factor's value
#' @param term the compounding factor term
#' @return CompoundFactor instance
#' 
#' @export
CompoundFactor <- function(value, term) {
    attr(value, 'term') <- term
    class(value) <- 'CompoundFactor'
    return(value)
}

#' @title as.CompoundFactor
#' 
#' @description
#' Compound factor
#' 
#' @export as.CompoundFactor
as.CompoundFactor <- function(object, ...) UseMethod('as.CompoundFactor', object)

#' @rdname as.CompoundFactor
#' @method as.CompoundFactor SpotRate
#' @S3method as.CompoundFactor SpotRate
as.CompoundFactor.SpotRate <- function(object) {
    fact <- attr(Compounding[[compounding(object)]], 'compound')(rate(object), term(object), dib(object))
    return( CompoundFactor(fact, term(object)) )
}

#' @rdname as.CompoundFactor
#' @method as.CompoundFactor CompoundFactor
#' @S3method as.CompoundFactor CompoundFactor
as.CompoundFactor.CompoundFactor <- function(object) {
    CompoundFactor(value=compound.factor(object), term=term(object))
}

#' @S3method * CompoundFactor
'*.CompoundFactor' <- function(cf.1, cf.2) {
    compound(cf.1, cf.2)
}

#' @S3method / CompoundFactor
'/.CompoundFactor' <- function(cf.2, cf.1) {
    discount(cf.1, cf.2)
}

#' @S3method all.equal CompoundFactor
all.equal.CompoundFactor <- function(target, current,
        tolerance=.Machine$double.eps^0.5, ...) {
    term(target) == term(current) &&
        abs(compound.factor(target) - compound.factor(current)) <= tolerance
}

#' @S3method print CompoundFactor
print.CompoundFactor <- function(object) {
    cat('compounding factor =', compound.factor(object),
        '\nterm =', term(object), '\n')
}

#' @S3method compound CompoundFactor
compound.CompoundFactor <- function(cf.1, cf.2) {
    CompoundFactor(compound.factor(cf.1) * compound.factor(cf.2),
        term(cf.1) + term(cf.2))
}

#' @S3method discount CompoundFactor
discount.CompoundFactor <- function(cf.1, cf.2) {
    CompoundFactor(compound.factor(cf.2) / compound.factor(cf.1),
        term(cf.2) - term(cf.1))
}

