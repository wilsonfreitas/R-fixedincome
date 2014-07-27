
#' The compounding object
#' 
#' @description
#' The \code{compounding} object abstracts the compounding regime used to 
#' discount or compound a spot rate.
#' 
#' @details
#' There are 3 compoundings:
#' \itemize{
#' \item \code{simple} for simple interest rate compounding
#' \deqn{1 + rt}
#' \item \code{discrete} for compounded interest rate compounding
#' \deqn{(1 + r)^t}
#' \item \code{continuous} for continuous interest rate compounding
#' \deqn{exp(rt)}
#' }
#' 
#' The \code{compounding} can be directly instanciated using one the functions:
#' \code{simpleCompounding}, \code{discreteCompounding}, 
#' \code{continuousCompounding}, or through \code{\link{as.compounding}}.
#' 
#' The \code{compounding} class has 2 methods:
#' \itemize{
#' \item \code{compound} to compound a rate for a given term.
#' \item \code{rates} to compute the implied rate for a compound factor
#' in a given term.
#' }
#' 
#' @name compounding-class
#' @examples
#' simpleCompounding() # or as.compounding("simple")
#' discreteCompounding() # or as.compounding("discrete")
#' continuousCompounding() # or as.compounding("continuous")
#' 
#' comp <- as.compounding("discrete")
#' compound(comp, 0.06, 1) # equals (1 + 0.06)^1 = 1.06
#' rates(comp, 1.06, 1) # equals 0.06
NULL

#' Create a compounding object
#' 
#' @description
#' Compounding object creation function.
#' 
#' @param obj one of those strings: \code{discrete}, \code{simple}, 
#' \code{continuous}
#' @param ... extra arguments
#' 
#' @return a compounding object
#' 
#' @name as.compounding
#' @examples
#' as.compounding('simple')
NULL

#' @rdname as.compounding
#' @export
as.compounding <- function(obj, ...) UseMethod('as.compounding', obj)

#' @rdname as.compounding
#' @export
as.compounding.character <- function(obj=c('discrete', 'simple', 'continuous'), ...) {
	obj <- match.arg(obj)
	switch(obj,
		discrete=discreteCompounding(),
		simple=simpleCompounding(),
		continuous=continuousCompounding())
}

#' @export
as.compounding.compounding <- identity

#' @rdname compounding-class
#' @export
discreteCompounding <- function() {
	comp <- 'discrete'
	attr(comp, 'compound') <- function (value, term) (1 + value)^(term)
	attr(comp, 'rates') <- function (value, term) value^(1/term) - 1
	class(comp) <- c('discrete', 'compounding')
	comp
}

#' @rdname compounding-class
#' @export
simpleCompounding <- function() {
	comp <- 'simple'
	attr(comp, 'compound') <- function (value, term) (1 + value*term)
	attr(comp, 'rates') <- function (value, term) (value - 1)*(1/term)
	class(comp) <- c('simple', 'compounding')
	comp
}

#' @rdname compounding-class
#' @export
continuousCompounding <- function() {
	comp <- 'continuous'
	attr(comp, 'compound') <- function (value, term) exp(value*term)
	attr(comp, 'rates') <- function (value, term) log(value)*(1/term)
	class(comp) <- c('continuous', 'compounding')
	comp
}

#' @details
#' If \code{obj} is a \code{\link{compounding-class}} instance the \code{value}
#' and \code{term} must be numeric values representing spot rate and 
#' compounding term, respectively.
#' 
#' @param value a numeric representing spot rate value
#' 
#' @rdname compound
#' @export
compound.compounding <- function(obj, value, term, ...) {
	compf <- attr(obj, 'compound')
	compf(value, term)
}

#' @details
#' If the \code{obj} argument is a \code{\link{compounding-class}} 
#' the function \code{rates.compounding} computes the implied rate 
#' for the given compounding and term.
#' 
#' @param value a numeric value representing a compounding factor
#' @param term a \code{\link{term-class}} instance
#' 
#' @rdname rates
#' @export
rates.compounding <- function(obj, value, term, ...) {
	compf <- attr(obj, 'rates')
	compf(value, term)
}

#' Compounding accessor
#' 
#' @description
#' Method to access the compounding attribute.
#' 
#' @param obj any object which has a compounding attribute
#' @param ... extra arguments
#' 
#' @return a compounding object
#' 
#' @name compounding
NULL

#' @rdname compounding
#' @export
compounding <- function(obj, ...) UseMethod('compounding', obj)

