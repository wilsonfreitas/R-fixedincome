
#' CurveInterpolation
#' 
#' Creates a curve interpolation class
#' 
#' @param curve curve
#' @param method \code{flatforward}, \code{linear}, \code{loglinear},
#' \code{spline}, \code{hermite}, \code{monotone}
#' @return \code{CurveInterpolation} class
#' @export
CurveInterpolation <- function(curve, method=flatforward) {
    attr(curve, 'method') <- method
    attr(curve, 'interp') <- tryCatch(
        do.call(method, list(curve)),
        error=function(e) {
            if (grepl('at least two', as.character(e)))
                NULL
            else
                stop(e)
        })
    # attr(curve, 'interp') <- interp.method$interp
    class(curve) <- c('CurveInterpolation', 'SpotRateCurve')
    invisible(curve)
}

#' Interpolation method
#' 
#' Interpolation method
#' 
#' @export
method <- function(obj, ...) UseMethod('method', obj)

#' @S3method method CurveInterpolation
method.CurveInterpolation <- function(curve) attr(curve, 'method')

#' @S3method [ CurveInterpolation
'[.CurveInterpolation' <- function(curve, term) {
    if (is.null(attr(curve, 'interp'))) {
        obj <- NextMethod("[")
    } else {
        interp <- attr(curve, 'interp')
        values <- interp(term)
        obj <- SpotRateCurve(values, term,
            dib=dib(curve), compounding=compounding(curve))
    }
    CurveInterpolation(obj, method(curve))
}

#' @S3method [<- CurveInterpolation
'[<-.CurveInterpolation' <- function(curve, i, value) {
    obj <- NextMethod("[<-")
    CurveInterpolation(obj, method(curve))
}

#' @export
flatforward <- function(curve) {
    prices <- (1 + rates(curve))^(terms(curve)/dib(curve))
    interp.coords <- xy.coords(terms(curve), log(prices))
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
    interp.coords <- xy.coords(terms(curve), rates(curve))
    interp.FUN <- approxfun(interp.coords, method='linear')
    function (term) interp.FUN(term)
}

#' @export
loglinear <- function(curve) {
    interp.coords <- xy.coords(terms(curve), log(rates(curve)))
    interp.FUN <- approxfun(interp.coords, method='linear')
    function (term) exp(interp.FUN(term))
}

#' @export
spline <- function(curve) {
    interp.coords <- xy.coords(terms(curve), rates(curve))
    interp.FUN <- splinefun(interp.coords, method='natural')
    function(term) interp.FUN(term)
}

#' @export
hermite <- function(curve) {
    interp.coords <- xy.coords(terms(curve), rates(curve))
    interp.FUN <- splinefun(interp.coords, method='monoH.FC')
    function(term) interp.FUN(term)
}

#' @export
monotone <- function(curve) {
    interp.coords <- xy.coords(terms(curve), rates(curve))
    interp.FUN <- splinefun(interp.coords, method='hyman')
    function(term) interp.FUN(term)
}

