
neighbors <- function(object, ...) UseMethod('neighbors', object)

neighbors.default <- function(object, ...)  stop('No default implementation')

neighbors.SpotRateCurve <- function(curve, term) {
    curve$terms[neighbors.indexes(curve, term)]
}

neighbors.indexes <- function(curve, term) {
    c(max(which(curve$terms <= term)), min(which(curve$terms >= term)))
}

interp.FlatForward <- function(curve, term) {
    log.PU <- curve$interp.FUN2(log(term/252))
    PU <- exp(log.PU)
    r <- PU^(252/term) - 1
    r
}

interp.FlatForward.prepare <- function(curve) {
    pus <- (1 + curve$rates)^(curve$terms/252)
    interp.coords <- xy.coords(log(curve$terms/252), log(pus))
    approxfun(interp.coords, method='linear')
}

# interp.FlatForward <- function(curve, term) {
#     idx <- neighbors.indexes(curve, term)
#     ir.u <- SpotRate(curve$rates[idx[2]], curve$terms[idx[2]])
#     ir.d <- SpotRate(curve$rates[idx[1]], curve$terms[idx[1]])
#     rate(flat.forward.interpolation(ir.d, ir.u, term))
# }
# 
# flat.forward.interpolation <- function(ir.d, ir.u, term) {
#     ir.fwd.adj <- as.SpotRate(forward.rate(ir.d, ir.u), term-term(ir.d))
#     new.cf <- as.CompoundFactor(ir.d) * as.CompoundFactor(ir.fwd.adj)
#     as.SpotRate(new.cf)
# }

interp.Linear <- function(curve, term) {
    curve$interp.FUN2(term)
}

interp.Linear.prepare <- function(curve) {
    interp.coords <- xy.coords(curve$terms, curve$rates)
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
