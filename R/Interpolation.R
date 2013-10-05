
neighbors <- function(object, ...) UseMethod('neighbors', object)

neighbors.default <- function(object, ...)  stop('No default implementation')

neighbors.SpotRateCurve <- function(curve, term) {
    curve$terms[neighbors.indexes(curve, term)]
}

neighbors.indexes <- function(curve, term) {
    c(max(which(curve$terms <= term)), min(which(curve$terms >= term)))
}

interp.FlatForward <- function(curve, term) {
    idx <- neighbors.indexes(curve, term)
    ir.u <- SpotRate(curve$rates[idx[2]], curve$terms[idx[2]])
    ir.d <- SpotRate(curve$rates[idx[1]], curve$terms[idx[1]])
    rate(flat.forward.interpolation(ir.d, ir.u, term))
}

flat.forward.interpolation <- function(ir.d, ir.u, term) {
    ir.fwd.adj <- as.SpotRate(forward.rate(ir.d, ir.u), term-term(ir.d))
    new.cf <- as.CompoundFactor(ir.d) * as.CompoundFactor(ir.fwd.adj)
    as.SpotRate(new.cf)
}

