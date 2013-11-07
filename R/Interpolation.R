
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
    attr(curve, 'method') <- method
    interp.method <- interpolationMethods[[method]]
    attr(curve, 'interp.FUN') <- tryCatch(
        interp.method$prepare(curve),
        error=function(e) NULL)
    attr(curve, 'interp') <- interp.method$interp
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
    if (is.null(attr(curve, 'interp.FUN'))) {
        obj <- NextMethod("[")
    } else {
        interp <- attr(curve, 'interp')
        values <- interp(curve, term, attr(curve, 'interp.FUN'))
        obj <- SpotRateCurve(values, term,
            dib=dib(curve), compounding=compounding(curve))
    }
    CurveInterpolation(obj, method(curve))
}

#' @S3method [<- CurveInterpolation
'[<-.CurveInterpolation' <- function(curve, i, value) {
    obj <- NextMethod("[<-")
    interp.method <- interpolationMethods[[method(curve)]]
    # attr(obj, 'interp.FUN') <- interp.method$prepare(obj)
    CurveInterpolation(obj, method(curve))
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
    ),
    spline=list(
        interp=function(curve, term, interp.FUN) interp.FUN(term),
        prepare=function(curve) {
            interp.coords <- xy.coords(terms(curve), rates(curve))
            splinefun(interp.coords, method='natural')
        }
    ),
    hermite=list(
        interp=function(curve, term, interp.FUN) interp.FUN(term),
        prepare=function(curve) {
            interp.coords <- xy.coords(terms(curve), rates(curve))
            splinefun(interp.coords, method='monoH.FC')
        }
    ),
    monotone=list(
        interp=function(curve, term, interp.FUN) interp.FUN(term),
        prepare=function(curve) {
            interp.coords <- xy.coords(terms(curve), rates(curve))
            splinefun(interp.coords, method='hyman')
        }
    )
)
