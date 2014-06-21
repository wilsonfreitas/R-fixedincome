#' Compounding class
#' 
#' @description
#' Compounding
#' 
#' @details
#' Compounding class
#' 
#' @param obj an instance of compounding class
#' @param value a numeric representing a compounding factor or a interest rate
#' @param term a valid term 
#' @param ... extra arguments
#' 
#' @name compounding-class
NULL

#' @rdname compounding-class
#' @export
compounding <- function(obj, ...) UseMethod('compounding', obj)

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

#' @rdname compounding-class
#' @export
compound.compounding <- function(obj, value, term, ...) {
	compf <- attr(obj, 'compound')
	compf(value, term)
}

#' @rdname compounding-class
#' @export
rates.compounding <- function(obj, value, term, ...) {
	compf <- attr(obj, 'rates')
	compf(value, term)
}

