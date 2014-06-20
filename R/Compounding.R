
#' @title compounding
#' 
#' @description
#' Compounding
#' 
#' @export compounding
compounding <- function(object, ...) UseMethod('compounding', object)

discreteCompounding <- function() {
	comp <- 'discrete'
	attr(comp, 'compound') <- function (value, term) (1 + value)^(term)
	attr(comp, 'rates') <- function (value, term) value^(1/term) - 1
	class(comp) <- c('discrete', 'compounding')
	comp
}

simpleCompounding <- function() {
	comp <- 'simple'
	attr(comp, 'compound') <- function (value, term) (1 + value*term)
	attr(comp, 'rates') <- function (value, term) (value - 1)*(1/term)
	class(comp) <- c('simple', 'compounding')
	comp
}

continuousCompounding <- function() {
	comp <- 'continuous'
	attr(comp, 'compound') <- function (value, term) exp(value*term)
	attr(comp, 'rates') <- function (value, term) log(value)*(1/term)
	class(comp) <- c('continuous', 'compounding')
	comp
}

compound.compounding <- function(comp, value, term) {
	compf <- attr(comp, 'compound')
	compf(value, term)
}

rates.compounding <- function(comp, value, term) {
	compf <- attr(comp, 'rates')
	compf(value, term)
}

