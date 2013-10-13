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
    that <- list()
    that$value <- value
    that$term <- term
    
    class(that) <- 'CompoundFactor'
    return(that)
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
    fact <- attr(Compounding[[object$compounding]], 'compound')(object$value, object$term, object$dib)
    return( CompoundFactor(fact, object$term) )
}

#' @rdname as.CompoundFactor
#' @method as.CompoundFactor CompoundFactor
#' @S3method as.CompoundFactor CompoundFactor
as.CompoundFactor.CompoundFactor <- function(object) {
    CompoundFactor(value=object$value, term=object$term)
}

#' @S3method * CompoundFactor
'*.CompoundFactor' <- function(cf.1, cf.2) {
    compound(cf.1, cf.2)
}

#' @S3method / CompoundFactor
'/.CompoundFactor' <- function(cf.2, cf.1) {
    CompoundFactor(cf.2$value/cf.1$value, cf.2$term - cf.1$term)
}

#' @S3method all.equal CompoundFactor
all.equal.CompoundFactor <- function(target, current, tolerance=.Machine$double.eps^0.5, ...) {
    target$term == current$term &&
        abs(target$value - current$value) <= tolerance
}

#' @S3method print CompoundFactor
print.CompoundFactor <- function(object) {
    cat('\ncompounding factor =', object$value, '\nterm =', object$term, '\n')
}

