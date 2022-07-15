#' Set/Get interpolation to SpotRateCurve
#'
#' Sets and gets interpolation method to the SpotRateCurve.
#'
#' @param x a SpotRateCurve object.
#' @param value a Interpolation object.
#' @param ... additional arguments. Currently unused.
#'
#' @return A Interpolatin object.
#' @aliases
#' interpolation,SpotRateCurve-method
#' interpolation<-,SpotRateCurve,Interpolation-method
#' interpolation<-,SpotRateCurve,NULL-method
#' @examples
#' terms <- c(1, 11, 26, 27, 28)
#' rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
#' curve <- spotratecurve(rates, terms, "discrete", "actual/365", "actual")
#' interpolation(curve) <- interp_flatforward()
#' interpolation(curve)
#' @export
setGeneric(
  "interpolation",
  function(x, ...) {
    standardGeneric("interpolation")
  }
)

#' @rdname interpolation
#' @export
setGeneric(
  "interpolation<-",
  function(x, value) {
    standardGeneric("interpolation<-")
  }
)

#' Create the interpolation function
#'
#' Creates the interpolation function to a SpotRateCurve object.
#'
#' @param object a Interpolation object.
#' @param x a SpotRateCurve object.
#' @param ... additional arguments. Currently unused.
#'
#' This method is used internally when the interpolation is set to a curve.
#' It uses the current state of the curve to build the interpolation function.
#' This is similar to call `approxfun` and `splinefun` to create functions that
#' perform interpolation of the given data points.
#'
#' This method shouldn't be directly called, it is for internal use only.
#'
#' @return
#' A `Interpolation` object with the slot `func` properly defined.
#' This slot is set with a `function` (closure) that executes
#' the interpolation method.
#'
#' @aliases
#' prepare_interpolation,FlatForward,SpotRateCurve-method
#' prepare_interpolation,HermiteSpline,SpotRateCurve-method
#' prepare_interpolation,Linear,SpotRateCurve-method
#' prepare_interpolation,LogLinear,SpotRateCurve-method
#' prepare_interpolation,MonotoneSpline,SpotRateCurve-method
#' prepare_interpolation,NaturalSpline,SpotRateCurve-method
#' prepare_interpolation,NelsonSiegel,SpotRateCurve-method
#' prepare_interpolation,NelsonSiegelSvensson,SpotRateCurve-method
#' @examples
#' terms <- c(1, 11, 26, 27, 28)
#' rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
#' curve <- spotratecurve(rates, terms, "discrete", "actual/365", "actual")
#' prepare_interpolation(interp_flatforward(), curve)
#' @export
setGeneric(
  "prepare_interpolation",
  function(object, x, ...) {
    standardGeneric("prepare_interpolation")
  }
)

#' Fit parametric interpolation functions
#'
#' Fits parametric interpolation functions like [NelsonSiegel-class] or
#' [NelsonSiegelSvensson-class].
#'
#' @param object a Interpolation object with initial parameters set.
#' @param x a SpotRateCurve object.
#' @param ... additional arguments. Currently unused.
#'
#' @return A `Interpolation` object.
#' @aliases
#' fit_interpolation,NelsonSiegel,SpotRateCurve-method
#' fit_interpolation,NelsonSiegelSvensson,SpotRateCurve-method
#' @examples
#' terms <- c(1, 11, 26, 27, 28)
#' rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
#' curve <- spotratecurve(rates, terms, "discrete", "actual/365", "actual")
#' fit_interpolation(interp_nelsonsiegel(0.1, 0.01, 0.01, 0.01), curve)
#' @export
setGeneric(
  "fit_interpolation",
  function(object, x, ...) {
    standardGeneric("fit_interpolation")
  }
)

setMethod(
  "interpolation",
  signature(x = "SpotRateCurve"),
  function(x) {
    x@interpolation
  }
)

setReplaceMethod(
  "interpolation",
  signature(x = "SpotRateCurve", value = "Interpolation"),
  function(x, value) {
    if (length(x) >= 2) {
      x@interpolation <- prepare_interpolation(value, x)
    } else {
      warning("interpolation<- not set - curve with less than 2 elements")
    }
    x
  }
)

setReplaceMethod(
  "interpolation",
  signature(x = "SpotRateCurve", value = "NULL"),
  function(x, value) {
    x@interpolation <- NULL
    x
  }
)

setMethod(
  "prepare_interpolation",
  signature(object = "FlatForward", x = "SpotRateCurve"),
  function(object, x, ...) {
    terms <- as.numeric(x@terms)
    prices <- compound(x)
    interp_coords <- xy.coords(terms, log(prices))
    interp_fun <- approxfun(interp_coords, method = "linear")
    dc <- x@daycount
    comp <- x@compounding
    object@func <- function(term_) {
      log.price <- interp_fun(term_)
      price <- exp(log.price)
      implied_rate(comp, as.numeric(toyears(dc, term(term_, "days"))), price)
    }
    object
  }
)

setMethod(
  "prepare_interpolation",
  signature(object = "Linear", x = "SpotRateCurve"),
  function(object, x, ...) {
    interp_coords <- xy.coords(as.numeric(x@terms), as.numeric(x))
    interp_fun <- approxfun(interp_coords, method = "linear")
    object@func <- function(term) interp_fun(term)
    object
  }
)

#' @export
setMethod(
  "prepare_interpolation",
  signature(object = "LogLinear", x = "SpotRateCurve"),
  function(object, x, ...) {
    interp_coords <- xy.coords(as.numeric(x@terms), log(as.numeric(x)))
    interp_fun <- approxfun(interp_coords, method = "linear")
    object@func <- function(term) exp(interp_fun(term))
    object
  }
)

#' @export
setMethod(
  "prepare_interpolation",
  signature(object = "NaturalSpline", x = "SpotRateCurve"),
  function(object, x, ...) {
    interp_coords <- xy.coords(as.numeric(x@terms), as.numeric(x))
    interp_fun <- splinefun(interp_coords, method = "natural")
    object@func <- function(term) interp_fun(term)
    object
  }
)

#' @export
setMethod(
  "prepare_interpolation",
  signature(object = "HermiteSpline", x = "SpotRateCurve"),
  function(object, x, ...) {
    interp_coords <- xy.coords(as.numeric(x@terms), as.numeric(x))
    interp_fun <- splinefun(interp_coords, method = "monoH.FC")
    object@func <- function(term) interp_fun(term)
    object
  }
)

#' @export
setMethod(
  "prepare_interpolation",
  signature(object = "MonotoneSpline", x = "SpotRateCurve"),
  function(object, x, ...) {
    interp_coords <- xy.coords(as.numeric(x@terms), as.numeric(x))
    interp_fun <- splinefun(interp_coords, method = "hyman")
    object@func <- function(term) interp_fun(term)
    object
  }
)