#' Creates a SpotRateCurve
#' 
#' SpotRateCurve abstracts a term structure class providing methods to operate 
#' on the term structure internals executing calculations on its interest rates.
#' Actually, the class SpotRateCurve represents a zero curve and for a given 
#' interpolation method (\code{interp} parameter), users can get zero interest 
#' rates for any day into the range deimited by its terms.
#'
#' @param rates a vector with the interest rates
#' @param terms a vector with the terms
#' @param interp defines which interpolation method to use: 
#' Linear, FlatForward, LogLinear, Hermite Cubic Spline, Monotone Cubic Spline,
#' Natural Cubi Spline
#' @export
#' @examples
#' # Creating a zero curve 
#' curve <- SpotRateCurve(c(0.08, 0.083, 0.089, 0.093, 0.095), c(0.5, 1, 1.5, 2, 2.5))
#' # Creating a zero curve with linear interpolation and 360 days in base (days per year)
#' curve <- SpotRateCurve(c(0.08, 0.083, 0.089, 0.093, 0.095), c(0.5, 1, 1.5, 2, 2.5), interp='Linear', dib=360)
SpotRateCurve <- function(rates, terms, interp='FlatForward', dib=252) {
    that <- list()
    stopifnot(length(rates) == length(terms))
    stopifnot(length(rates) > 2)
    that$rates <- rates
    that$terms <- terms
    that$dib <- dib
    that$interp.method <- interp
    that$interp.FUN <- eval(parse(text=paste('interp.', interp, sep='')))
    that$interp.FUN2 <- eval(parse(text=paste('interp.', interp, '.prepare', sep='')))(that)
    class(that) <- 'SpotRateCurve'
    return(that)
}

#' @export
as.SpotRateCurve <- function(object, ...) UseMethod('as.SpotRateCurve', object)

#' @export
as.SpotRateCurve.data.frame <- function(df, ...) {
    SpotRateCurve(terms=df$terms, rates=df$rates)
}

#' @export
as.SpotRateCurve.matrix <- function(df, ...) {
    SpotRateCurve(terms=df[,'terms'], rates=df[,'rates'])
}

#' @export
as.data.frame.SpotRateCurve <- function(curve, ...) {
    data.frame(terms=curve$terms, rates=curve$rates, list(...))
}

#' @export
terms <- function(object, ...) UseMethod('terms', object)

#' @export
terms.SpotRateCurve <- function(object) object$terms

#' @export
rates <- function(object, ...) UseMethod('rates', object)

#' @export
rates.SpotRateCurve <- function(object) object$rates

#' @export
length.SpotRateCurve <- function(object) length(object$terms)

#' @export
interp <- function(object, ...) UseMethod('interp', object)

#' @export
interp.default <- function(object, ...)  stop('No default implementation')

#' @export
interp.SpotRateCurve <- function(curve, term) curve$interp.FUN(curve, term)

#' @export
'[.SpotRateCurve' <- function(object, term, forward.term=NULL, to.term=NULL) {
    if ( !is.null(to.term) || !is.null(forward.term) )
        return( forward.rate(object, term, forward.term, to.term) )
    SpotRate(object$interp.FUN(object, term), term, dib=object$dib)
}

#' @export
forward.rate.SpotRateCurve <- function(curve, from.term, forward.term=1, to.term=NULL) {
    ir.i <- SpotRate(curve$interp.FUN(curve, from.term), from.term)
    if ( is.null(to.term) )
        to.term <- from.term + forward.term
    ir.p <- SpotRate(curve$interp.FUN(curve, to.term), to.term)
    forward.rate(ir.i, ir.p)
}

#' @export
plot.SpotRateCurve <- function(curve, ...) {
    plot(curve$terms, curve$rates, ...)
}

#' @export
'insert<-' <- function(object, ...) UseMethod('insert<-', object)

#' @export
'insert<-.SpotRateCurve' <- function(object, value) {
    object$terms <- append(object$terms, value$term)
    object$rates <- append(object$rates, value$value)
    idx <- order(object$terms)
    object$terms <- object$terms[idx]
    object$rates <- object$rates[idx]
    object
}


