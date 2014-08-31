
#' @export
flatforward <- function(curve) {
	.terms <- terms(curve, as.x=TRUE)
	prices <- compound(curve, .terms)
	interp.coords <- xy.coords(.terms, log(prices))
	interp.FUN <- approxfun(interp.coords, method='linear')
	dc <- daycount(curve)
	comp <- compounding(curve)
	function (term) {
		log.price <- interp.FUN(term)
		price <- exp(log.price)
		rates(comp, price, timefactor(dc, term, units(curve)))
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

