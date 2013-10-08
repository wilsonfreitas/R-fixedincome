#' Creates a SpotRateCurve
#' 
#' SpotRateCurve abstracts a term structure class providing methods to operate 
#' on the term structure.
#'
#' @param rates a vector with the interest rates
#' @param terms a vector with the terms
#' @param interp defines which interpolation method to use: Linear, 
#' FlatForward, LogLinear, 
#' @export
#' @examples
#' # holidays has iso-formated dates
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' # For empty calendar just pass nothing
#' cal <- Calendar()
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

as.SpotRateCurve <- function(object, ...) UseMethod('as.SpotRateCurve', object)

as.SpotRateCurve.data.frame <- function(df, ...) {
    SpotRateCurve(terms=df$terms, rates=df$rates)
}

as.SpotRateCurve.matrix <- function(df, ...) {
    SpotRateCurve(terms=df[,'terms'], rates=df[,'rates'])
}

as.data.frame.SpotRateCurve <- function(curve, ...) {
    data.frame(terms=curve$terms, rates=curve$rates, list(...))
}

terms <- function(object, ...) UseMethod('terms', object)

terms.SpotRateCurve <- function(object) object$terms

rates <- function(object, ...) UseMethod('rates', object)

rates.SpotRateCurve <- function(object) object$rates

length.SpotRateCurve <- function(object) length(object$terms)

interp <- function(object, ...) UseMethod('interp', object)

interp.default <- function(object, ...)  stop('No default implementation')

interp.SpotRateCurve <- function(curve, term) curve$interp.FUN(curve, term)

'[.SpotRateCurve' <- function(object, term, forward.term=NULL, to.term=NULL) {
    if ( !is.null(to.term) || !is.null(forward.term) )
        return( forward.rate(object, term, forward.term, to.term) )
    SpotRate(object$interp.FUN(object, term), term, dib=object$dib)
}

forward.rate.SpotRateCurve <- function(curve, from.term, forward.term=1, to.term=NULL) {
    ir.i <- SpotRate(curve$interp.FUN(curve, from.term), from.term)
    if ( is.null(to.term) )
        to.term <- from.term + forward.term
    ir.p <- SpotRate(curve$interp.FUN(curve, to.term), to.term)
    forward.rate(ir.i, ir.p)
}

plot.SpotRateCurve <- function(curve, ...) {
    plot(curve$terms, curve$rates, ...)
}

'insert<-' <- function(object, ...) UseMethod('insert<-', object)

'insert<-.SpotRateCurve' <- function(object, value) {
    object$terms <- append(object$terms, value$term)
    object$rates <- append(object$rates, value$value)
    idx <- order(object$terms)
    object$terms <- object$terms[idx]
    object$rates <- object$rates[idx]
    object
}


