#'
#' InterestRate Curve
#' 

SpotRateCurve <- function(rates, terms, interp='FlatForward') {
    that <- list()
    stopifnot(length(rates) == length(terms))
    that$rates <- rates
    that$terms <- terms
    that$interp.method <- interp
    that$interp.FUN <- eval(parse(text=paste('interp.', interp, sep='')))
    class(that) <- 'SpotRateCurve'
    return(that)
}

terms.SpotRateCurve <- function(object) object$terms

rates <- function(object, ...) UseMethod('rates', object)

rates.SpotRateCurve <- function(object) object$rates

length.SpotRateCurve <- function(object) {
	length(object$terms)
}

'[.SpotRateCurve' <- function(object, term, forward.term=NA, to.term=NA) {
    if ( !is.na(to.term) || !is.na(forward.term) )
        return( forward.rate(object, term, to.term, forward.term) )
    SpotRate(object$interp.FUN(object, term), term)
}

forward.rate.SpotRateCurve <- function(curve, from.term, to.term=NULL, forward.term=1) {
    if (from.term == 1) return( curve[1] )
    ir.i <- curve$interp.FUN(curve, from.term)
    if ( !is.null(to.term) )
        ir.p <- curve$interp.FUN(curve, to.term)
    else
        ir.p <- curve$interp.FUN(curve, from.term+1)
    as.SpotRate(ir.p, ir.i)
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


