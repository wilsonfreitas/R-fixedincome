
#' @export
setGeneric(
  "interpolation",
  function(x, ...) {
    standardGeneric("interpolation")
  }
)

#' @export
setGeneric(
  "interpolation<-",
  function(x, value, ...) {
    standardGeneric("interpolation<-")
  }
)

#' @export
setGeneric(
  "prepare_interpolation",
  function(object, x, ...) {
    standardGeneric("prepare_interpolation")
  }
)

#' @export
setGeneric(
  "fit_interpolation",
  function(object, x, ...) {
    standardGeneric("fit_interpolation")
  }
)

#' @export
setMethod(
  "interpolation",
  signature(x = "SpotRateCurve"),
  function(x) {
    x@interpolation
  }
)

#' @export
setReplaceMethod(
  "interpolation",
  signature(x = "SpotRateCurve", value = "Interpolation"),
  function(x, value) {
    x@interpolation <- prepare_interpolation(value, x)
    x
  }
)

#' @export
setReplaceMethod(
  "interpolation",
  signature(x = "SpotRateCurve", value = "NULL"),
  function(x, value) {
    x@interpolation <- value
    x
  }
)

#' @export
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
    object@func <- function(term) {
      log.price <- interp_fun(term)
      price <- exp(log.price)
      rates(comp, toyears(dc, term, "days"), price)
    }
    object
  }
)

#' @export
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

ns <- function(t, b1, b2, b3, l1) {
  b1 +
    b2 * (1 - exp(-l1 * t)) / (l1 * t) +
    b3 * ((1 - exp(-l1 * t)) / (l1 * t) - exp(-l1 * t))
}

#' @export
setMethod(
  "prepare_interpolation",
  signature(object = "NelsonSiegel", x = "SpotRateCurve"),
  function(object, x, ...) {
    object@func <- function(term) {
      ns(term, object@beta1, object@beta2, object@beta3, object@lambda1)
    }
    object
  }
)

nss <- function(t, b1, b2, b3, b4, l1, l2) {
  ns(t, b1, b2, b3, l1) + b4 * ((1 - exp(-l2 * t)) / (l2 * t) - exp(-l2 * t))
}

#' @export
setMethod(
  "prepare_interpolation",
  signature(object = "NelsonSiegelSvensson", x = "SpotRateCurve"),
  function(object, x, ...) {
    object@func <- function(term) {
      nss(
        term, object@beta1, object@beta2, object@beta3, object@beta4,
        object@lambda1, object@lambda2
      )
    }
    object
  }
)

#' @export
setMethod(
  "fit_interpolation",
  signature(object = "NelsonSiegel", x = "SpotRateCurve"),
  function(object, x, ...) {
    par <- parameters(object)
    res <- optim(par, function(par, x) {
      interpolation(x) <- do.call(interp_nelsonsiegel, as.list(par))
      interpolation_error(x)
    }, method = "BFGS", x = x)
    do.call(interp_nelsonsiegel, as.list(res$par))
  }
)

#' @export
setMethod(
  "fit_interpolation",
  signature(object = "NelsonSiegelSvensson", x = "SpotRateCurve"),
  function(object, x, ...) {
    par <- parameters(object)
    res <- optim(par, function(par, x) {
      interpolation(x) <- do.call(interp_nelsonsiegelsvensson, as.list(par))
      interpolation_error(x)
    }, method = "BFGS", x = x)
    do.call(interp_nelsonsiegelsvensson, as.list(res$par))
  }
)