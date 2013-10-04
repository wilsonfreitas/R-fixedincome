#'
#' InterestRate Curve
#' 

SpotRateCurve <- function(rates, terms, interp.FUN=interp.FlatForward) {
    that <- list()
    stopifnot(length(rates) == length(terms))
    that$rates <- rates
    that$terms <- terms
    that$interp.FUN <- interp.FUN
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

neighbors <- function(object, ...) UseMethod('neighbors', object)

neighbors.default <- function(object, ...)  stop('No default implementation')

neighbors.SpotRateCurve <- function(curve, term) {
    curve$terms[c(max(which(curve$terms <= term)), min(which(curve$terms >= term)))]
}

interp.FlatForward <- function(object, ...) UseMethod('interp.FlatForward', object)

interp.FlatForward.default <- function(object, ...)  stop('No default implementation')

interp.FlatForward.SpotRateCurve <- function(curve, term) {
    if ( any( idx <- curve$terms == term ) ) {
        return(curve$rates[idx])
    } else {
        idx.u <- min(which(curve$terms > term))
        idx.d <- max(which(curve$terms < term))
        ir.u <- SpotRate(curve$rates[idx.u], curve$terms[idx.u])
        ir.d <- SpotRate(curve$rates[idx.d], curve$terms[idx.d])
        ir.fwd <- forward.rate(ir.d, ir.u)
        # use forward rate with new term
        ir.fwd.adj <- as.SpotRate(ir.fwd, term-term(ir.d))
        new.cf <- as.CompoundFactor(ir.d) * as.CompoundFactor(ir.fwd.adj)
        return( rate(as.SpotRate(new.cf)) )
    }
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


