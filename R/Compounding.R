
#' @title compounding
#' 
#' @description
#' Compounding
#' 
#' @export compounding
compounding <- function(object, ...) UseMethod('compounding', object)

#' @rdname compounding
#' @method compounding SpotRate
#' @S3method compounding SpotRate
compounding.SpotRate <- function (object) object$compounding

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

