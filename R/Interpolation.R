
neighbors <- function(object, ...) UseMethod('neighbors', object)

neighbors.default <- function(object, ...)  stop('No default implementation')

neighbors.SpotRateCurve <- function(curve, term) {
    curve$terms[neighbors.indexes(curve, term)]
}

neighbors.indexes <- function(curve, term) {
    c(max(which(curve$terms <= term)), min(which(curve$terms >= term)))
}

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
    exp(curve$interp.FUN2(log(term)))
}

interp.LogLinear.prepare <- function(curve) {
    interp.coords <- xy.coords(log(curve$terms), log(curve$rates))
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
