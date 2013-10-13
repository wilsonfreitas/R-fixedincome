
#' neighbors
#' 
#' Neighbors for a given term
#' 
#' @export
neighbors <- function(object, ...) UseMethod('neighbors', object)

#' @S3method neighbors SpotRateCurve
neighbors.SpotRateCurve <- function(curve, term) {
    curve$terms[neighbors.indexes(curve, term)]
}

# return the neighbors indexes in a SpotRateCurve
neighbors.indexes <- function(curve, term) {
    c(max(which(curve$terms <= term)), min(which(curve$terms >= term)))
}

#' interp
#' 
#' Interpolate curves
#' 
#' @export
interp <- function(object, ...) UseMethod('interp', object)

#' @rdname interp
#' @method interp SpotRateCurve
#' @S3method interp SpotRateCurve
interp.SpotRateCurve <- function(curve, term) curve$interp.FUN(curve, term)

interp.FlatForward <- function(curve, term) {
    log.PU <- curve$interp.FUN2(term/curve$dib)
    PU <- exp(log.PU)
    r <- PU^(curve$dib/term) - 1
    r
}

interp.FlatForward.prepare <- function(curve) {
    pus <- (1 + curve$rates)^(curve$terms/curve$dib)
    interp.coords <- xy.coords(curve$terms/curve$dib, log(pus))
    approxfun(interp.coords, method='linear')
}

interp.Linear <- function(curve, term) {
    curve$interp.FUN2(term)
}

interp.Linear.prepare <- function(curve) {
    interp.coords <- xy.coords(curve$terms, curve$rates)
    approxfun(interp.coords, method='linear')
}

interp.LogLinear <- function(curve, term) {
    exp(curve$interp.FUN2(term))
}

interp.LogLinear.prepare <- function(curve) {
    interp.coords <- xy.coords(curve$terms, log(curve$rates))
    approxfun(interp.coords, method='linear')
}

interp.Spline <- function(curve, term) {
    curve$interp.FUN2(term)
}

interp.Spline.prepare <- function(curve) {
    interp.coords <- xy.coords(curve$terms, curve$rates)
    splinefun(interp.coords, method='natural')
}

interp.Hermite <- function(curve, term) {
    curve$interp.FUN2(term)
}

interp.Hermite.prepare <- function(curve) {
    interp.coords <- xy.coords(curve$terms, curve$rates)
    splinefun(interp.coords, method='monoH.FC')
}

interp.Monotone <- function(curve, term) {
    curve$interp.FUN2(term)
}

interp.Monotone.prepare <- function(curve) {
    interp.coords <- xy.coords(curve$terms, curve$rates)
    splinefun(interp.coords, method='hyman')
}
