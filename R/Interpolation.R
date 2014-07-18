
#' @export
flatforward <- function(curve) {
    prices <- (1 + rates(curve))^(terms(curve, as.x=TRUE)/dib(curve))
    interp.coords <- xy.coords(terms(curve, as.x=TRUE), log(prices))
    interp.FUN <- approxfun(interp.coords, method='linear')
    dib <- dib(curve)
    function (term) {
        log.price <- interp.FUN(term)
        price <- exp(log.price)
        price^(dib/term) - 1
    }
}

#' @export
linear <- function(curve) {
    interp.coords <- xy.coords(terms(curve, as.x=TRUE), rates(curve))
    interp.FUN <- approxfun(interp.coords, method='linear')
    function (term) interp.FUN(term)
}

#' @export
log.linear <- function(curve) {
    interp.coords <- xy.coords(terms(curve, as.x=TRUE), log(rates(curve)))
    interp.FUN <- approxfun(interp.coords, method='linear')
    function (term) exp(interp.FUN(term))
}

#' @export
natural.spline <- function(curve) {
    interp.coords <- xy.coords(terms(curve, as.x=TRUE), rates(curve))
    interp.FUN <- splinefun(interp.coords, method='natural')
    function(term) interp.FUN(term)
}

#' @export
hermite.spline <- function(curve) {
    interp.coords <- xy.coords(terms(curve, as.x=TRUE), rates(curve))
    interp.FUN <- splinefun(interp.coords, method='monoH.FC')
    function(term) interp.FUN(term)
}

#' @export
monotone.spline <- function(curve) {
    interp.coords <- xy.coords(terms(curve, as.x=TRUE), rates(curve))
    interp.FUN <- splinefun(interp.coords, method='hyman')
    function(term) interp.FUN(term)
}

