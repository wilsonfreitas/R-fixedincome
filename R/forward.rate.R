
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
forward.rate.SpotRateCurve <- function(curve, from.term, to.term=NULL,
    forward.term=NULL) {

    stopifnot(!is.null(to.term) || !is.null(forward.term))
    to.term <- if (!is.null(to.term)) to.term else from.term + forward.term
    stopifnot(to.term > from.term)
    sr.first <- SpotRate(curve[from.term], from.term, dib(curve),
        compounding(curve))
    sr.second <- SpotRate(curve[to.term], to.term, dib(curve),
        compounding(curve))
    forward.rate(sr.first, sr.second)
}

