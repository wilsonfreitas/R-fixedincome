#' Interpolation classes
#'
#' Classes that implement interpolation methods to be used with `SpotRateCurve`
#' objects.
#'
#' \itemize{
#'   \item FlatForward
#'   \item Linear
#'   \item LogLinear
#'   \item NaturalSpline
#'   \item HermiteSpline
#'   \item MonotoneSpline
#'   \item NelsonSiegel
#'   \item NelsonSiegelSvensson
#' }
#'
#' Every class that implement a interpolation method inherits the
#' \code{Interpolation} class.
#'
#' @aliases FlatForward-class Linear-class LogLinear-class NaturalSpline-class
#'          HermiteSpline-class MonotoneSpline-class NelsonSiegel-class
#'          NelsonSiegelSvensson-class
#'
#' @export
setClass(
  "Interpolation",
  slots = c(
    func = "ANY"
  ),
  contains = "character"
)

#' @export
setClass(
  "FlatForward",
  contains = "Interpolation"
)

#' @export
setClass(
  "Linear",
  contains = "Interpolation"
)

#' @export
setClass(
  "LogLinear",
  contains = "Interpolation"
)

#' @export
setClass(
  "NaturalSpline",
  contains = "Interpolation"
)

#' @export
setClass(
  "HermiteSpline",
  contains = "Interpolation"
)

#' @export
setClass(
  "MonotoneSpline",
  contains = "Interpolation"
)

#' @export
setClass(
  "NelsonSiegel",
  slots = c(
    beta1 = "numeric",
    beta2 = "numeric",
    beta3 = "numeric",
    lambda1 = "numeric"
  ),
  contains = "Interpolation"
)

#' @export
setClass(
  "NelsonSiegelSvensson",
  slots = c(
    beta1 = "numeric",
    beta2 = "numeric",
    beta3 = "numeric",
    beta4 = "numeric",
    lambda1 = "numeric",
    lambda2 = "numeric"
  ),
  contains = "Interpolation"
)

#' Interpolates a `SpotRateCurve`
#'
#' This method is internally used by the interpolation engine.
#' The interpolation function is created by calling `prepare_interpolation`
#' method.
#' This method creates the interpolation funcion with the current state of the
#' curve object and sets this function to the `func` slot.
#'
#' @keywords internal
#' @aliases interpolate,Interpolation,numeric-method
#' @aliases interpolate,NelsonSiegel,numeric-method
#' @aliases interpolate,NelsonSiegelSvensson,numeric-method
#'
#' @export
setGeneric(
  "interpolate",
  function(object, x, ...) {
    standardGeneric("interpolate")
  }
)

setMethod(
  "interpolate",
  signature(object = "Interpolation", x = "numeric"),
  function(object, x, ...) {
    object@func(x)
  }
)

nss <- function(t, b1, b2, b3, b4, l1, l2) {
  ns(t, b1, b2, b3, l1) + b4 * ((1 - exp(-l2 * t)) / (l2 * t) - exp(-l2 * t))
}

setMethod(
  "interpolate",
  signature(object = "NelsonSiegelSvensson", x = "numeric"),
  function(object, x, ...) {
    nss(
      x, object@beta1, object@beta2, object@beta3, object@beta4,
      object@lambda1, object@lambda2
    )
  }
)

ns <- function(t, b1, b2, b3, l1) {
  b1 +
    b2 * (1 - exp(-l1 * t)) / (l1 * t) +
    b3 * ((1 - exp(-l1 * t)) / (l1 * t) - exp(-l1 * t))
}

setMethod(
  "interpolate",
  signature(object = "NelsonSiegel", x = "numeric"),
  function(object, x, ...) {
    ns(
      x, object@beta1, object@beta2, object@beta3, object@lambda1
    )
  }
)

#' @export
setMethod(
  "show",
  "Interpolation",
  function(object) {
    cat("<Interpolation:", as.character(object), "\b>", "\n")
    invisible(object)
  }
)

#' Create Interpolation objects
#'
#' Functions to create intepolation objects.
#'
#' @param beta1 a single numeric
#' @param beta2 a single numeric
#' @param beta3 a single numeric
#' @param beta4 a single numeric
#' @param lambda1 a single numeric
#' @param lambda2 a single numeric
#'
#' @details
#' `interp_flatforward` creates a `FlatForward` interpolation object.
#'
#' `interp_linear` creates a `Linear` interpolation object.
#'
#' `interp_loglinear` creates a `LogLinear` interpolation object.
#'
#' `interp_naturalspline` creates a `NaturalSpline` interpolation object.
#'
#' `interp_hermitespline` creates a `HermiteSpline` interpolation object.
#'
#' `interp_monotonespline` creates a `MonotoneSpline` interpolation object.
#'
#' `interp_nelsonsiegel` creates a `NelsonSiegel` interpolation object.
#' The arguments `beta1`, `beta2`, `beta3`, `lambda1` are the paremeters of
#' the Nelson-Siegel model for term structure.
#'
#' `interp_nelsonsiegelsvensson` creates a `NelsonSiegelSvensson`
#' interpolation object.
#' The arguments `beta1`, `beta2`, `beta3`, `beta4`, `lambda1`, `lambda2` are
#' the paremeters of Svensson's extension to Nelson-Siegel the model for
#' term structure.
#'
#' @name interpolation-constructor
#'
#' @examples
#' terms <- c(1, 11, 26, 27, 28)
#' rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
#'
#' curve <- spotratecurve(rates, terms, "discrete", "actual/365", "actual")
#'
#' interpolation(curve) <- interp_flatforward()
#'
#' curve[[1:10]]
NULL

#' @rdname interpolation-constructor
#' @export
interp_flatforward <- function() new("FlatForward", "flatforward")

#' @rdname interpolation-constructor
#' @export
interp_linear <- function() new("Linear", "linear")

#' @rdname interpolation-constructor
#' @export
interp_loglinear <- function() new("LogLinear", "loglinear")

#' @rdname interpolation-constructor
#' @export
interp_naturalspline <- function() new("NaturalSpline", "naturalspline")

#' @rdname interpolation-constructor
#' @export
interp_hermitespline <- function() new("HermiteSpline", "hermitespline")

#' @rdname interpolation-constructor
#' @export
interp_monotonespline <- function() new("MonotoneSpline", "monotonespline")

#' @rdname interpolation-constructor
#' @export
interp_nelsonsiegel <- function(beta1, beta2, beta3, lambda1) {
  new("NelsonSiegel", "nelsonsiegel",
    beta1 = beta1,
    beta2 = beta2,
    beta3 = beta3,
    lambda1 = lambda1
  )
}

#' @rdname interpolation-constructor
#' @export
interp_nelsonsiegelsvensson <- function(beta1, beta2, beta3, beta4,
                                        lambda1, lambda2) {
  new("NelsonSiegelSvensson", "nelsonsiegelsvensson",
    beta1 = beta1,
    beta2 = beta2,
    beta3 = beta3,
    beta4 = beta4,
    lambda1 = lambda1,
    lambda2 = lambda2
  )
}

#' @export
setMethod(
  "show",
  signature(object = "NelsonSiegelSvensson"),
  function(object) {
    cat("<Interpolation:", as.character(object), "\b>", "\n", "\bParameters:\n")
    print(parameters(object), digits = 2)
    invisible(object)
  }
)

#' @export
setMethod(
  "show",
  signature(object = "NelsonSiegel"),
  function(object) {
    cat("<Interpolation:", as.character(object), "\b>", "\n", "\bParameters:\n")
    print(parameters(object), digits = 2)
    invisible(object)
  }
)

#' Get/Set parameters of the interpolation models
#'
#' Gets parameters of parametric interpolation models like
#' [NelsonSiegel-class] and [NelsonSiegelSvensson-class].
#'
#' @param x a Interpolation object.
#' @param value a named vector with parameters.
#' @param ... additional arguments. Currently unused.
#' 
#' The argument `value` must be named with the models' parameter names.
#' The given parameters will be replaced the missing ones will keep
#' the original values.
#'
#' @return A named vector with parameters of the models.
#' @aliases
#' parameters,NelsonSiegel-method
#' parameters,NelsonSiegelSvensson-method
#' parameters<-,NelsonSiegel-method
#' parameters<-,NelsonSiegelSvensson-method
#' @examples
#' terms <- c(1, 11, 26, 27, 28)
#' rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
#' curve <- spotratecurve(rates, terms, "discrete", "actual/365", "actual")
#' model <- fit_interpolation(interp_nelsonsiegel(0.1, 0.01, 0.01, 0.01), curve)
#' parameters(model)
#' 
#' # only beta1 is updated 
#' parameters(model) <- c(beta1 = 0.0719)
#' @export
setGeneric(
  "parameters",
  function(x, ...) {
    standardGeneric("parameters")
  }
)

#' @rdname parameters
#' @export
setGeneric(
  "parameters<-",
  function(x, value) {
    standardGeneric("parameters<-")
  }
)

setMethod(
  "parameters",
  signature(x = "NelsonSiegel"),
  function(x, ...) {
    c(beta1 = x@beta1, beta2 = x@beta2, beta3 = x@beta3, lambda1 = x@lambda1)
  }
)

setMethod(
  "parameters",
  signature(x = "NelsonSiegelSvensson"),
  function(x, ...) {
    c(
      beta1 = x@beta1, beta2 = x@beta2, beta3 = x@beta3, beta4 = x@beta4,
      lambda1 = x@lambda1, lambda2 = x@lambda2
    )
  }
)

setReplaceMethod(
  "parameters",
  signature(x = "NelsonSiegel", value = "numeric"),
  function(x, value) {
    if (!is.na(value["beta1"])) x@beta1 <- unname(value["beta1"])
    if (!is.na(value["beta2"])) x@beta2 <- unname(value["beta2"])
    if (!is.na(value["beta3"])) x@beta3 <- unname(value["beta3"])
    if (!is.na(value["lambda1"])) x@lambda1 <- unname(value["lambda1"])
    x
  }
)

setReplaceMethod(
  "parameters",
  signature(x = "NelsonSiegelSvensson", value = "numeric"),
  function(x, value) {
    if (!is.na(value["beta1"])) x@beta1 <- unname(value["beta1"])
    if (!is.na(value["beta2"])) x@beta2 <- unname(value["beta2"])
    if (!is.na(value["beta3"])) x@beta3 <- unname(value["beta3"])
    if (!is.na(value["beta4"])) x@beta4 <- unname(value["beta4"])
    if (!is.na(value["lambda1"])) x@lambda1 <- unname(value["lambda1"])
    if (!is.na(value["lambda2"])) x@lambda2 <- unname(value["lambda2"])
    x
  }
)