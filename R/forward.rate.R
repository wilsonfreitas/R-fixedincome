
#' @title forward.rate
#' 
#' @description
#' Forward rate
#' 
#' @export
forward.rate <- function(object, ...) UseMethod('forward.rate', object)

#' @rdname forward.rate
#' @method forward.rate SpotRate
#' @S3method forward.rate SpotRate
forward.rate.SpotRate <- function (object, other) {
    if (object$term > other$term)
        stop('First parameter must have the smaller term.')
    fact.1 <- as.CompoundFactor(object)
    fact.2 <- as.CompoundFactor(other)
    as.SpotRate(fact.2/fact.1)
}

#' @rdname forward.rate
#' @method forward.rate SpotRateCurve
#' @S3method forward.rate SpotRateCurve
forward.rate.SpotRateCurve <- function(curve, from.term, forward.term=1, to.term=NULL) {
    ir.i <- SpotRate(curve$interp.FUN(curve, from.term), from.term)
    if ( is.null(to.term) )
        to.term <- from.term + forward.term
    ir.p <- SpotRate(curve$interp.FUN(curve, to.term), to.term)
    forward.rate(ir.i, ir.p)
}

