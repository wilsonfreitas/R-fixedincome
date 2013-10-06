#'
#' InterestRate Curve
#' 

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

as.data.frame.SpotRateCurve <- function(curve, ...) {
    data.frame(terms=curve$terms, rates=curve$rates, list(...))
}

plot.SpotRateCurve <- function(curve, ...) {
    plot(curve$terms, curve$rates, list(...))
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


