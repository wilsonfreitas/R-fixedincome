
#' CurveInterpolation
#' 
#' Creates a curve interpolation class
#' 
#' @param curve curve
#' @param method \code{flatforward}, \code{linear}, \code{loglinear},
#' \code{spline}, \code{hermite}, \code{monotone}
#' @return \code{CurveInterpolation} class
#' @export
CurveInterpolation <- function(curve, method='flatforward') {
    curve$method <- method
    interp.method <- interpolationMethods[[method]]
    curve$interp.FUN <- interp.method$prepare(curve)
    curve$interp <- interp.method$interp
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
method.CurveInterpolation <- function(curve) curve$method

#' @S3method [ CurveInterpolation
'[.CurveInterpolation' <- function(curve, term) {
    # if (any(terms(curve) %in% term))
    #     NextMethod("[")
    #  else
    curve$interp(curve, term, curve$interp.FUN)
}

#' interp
#' 
#' Interpolate curves
#' 
#' @export
interp <- function(object, ...) UseMethod('interp', object)

interpolationMethods <- list(
    flatforward=list(
        interp=function(curve, term, interp.FUN) {
            log.price <- interp.FUN(term)
            price <- exp(log.price)
            price^(dib(curve)/term) - 1
        },
        prepare=function(curve) {
            prices <- (1 + rates(curve))^(terms(curve)/dib(curve))
            interp.coords <- xy.coords(terms(curve), log(prices))
            approxfun(interp.coords, method='linear')
        }
    ),
    linear=list(
        interp=function(curve, term, interp.FUN) interp.FUN(term),
        prepare=function(curve) {
            interp.coords <- xy.coords(terms(curve), rates(curve))
            approxfun(interp.coords, method='linear')
        }
    ),
    loglinear=list(
        interp=function(curve, term, interp.FUN) exp(interp.FUN(term)),
        prepare=function(curve) {
            interp.coords <- xy.coords(terms(curve), log(rates(curve)))
            approxfun(interp.coords, method='linear')
        }
    )
)

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
