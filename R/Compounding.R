
#' @title compounding
#' 
#' @description
#' Compounding
#' 
#' @export compounding
compounding <- function(object, ...) UseMethod('compounding', object)

#' @rdname compounding
#' @method compounding SpotRateCurve
#' @S3method compounding SpotRateCurve
compounding.SpotRateCurve <- function (object) attr(object, 'compounding')

Compounding <- (function () {
    compounded <- 'compounded'
    attr(compounded, 'compound') <- function (value, term, dib) (1 + value)^(term/dib)
    attr(compounded, 'implied.rate') <- function (value, term, dib) value^(dib/term) - 1
    simple <- 'simple'
    attr(simple, 'compound') <- function (value, term, dib) (1 + value*term/dib)
    attr(simple, 'implied.rate') <- function (value, term, dib) (value - 1)*(dib/term)
    continuous <- 'continuous'
    attr(continuous, 'compound') <- function (value, term, dib) exp(value*term/dib)
    attr(continuous, 'implied.rate') <- function (value, term, dib) log(value)*(dib/term)
    list(
        compounded=compounded,
        simple=simple,
        continuous=continuous
    )
})()

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

