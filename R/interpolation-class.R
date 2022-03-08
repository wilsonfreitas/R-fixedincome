#' Interpolation classes
#'
#' Classes that implement interpolation methods to be used with SpotRateCurve
#' objects.
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

#' @export
setGeneric(
  "interpolate",
  function(object, x, ...) {
    standardGeneric("interpolate")
  }
)

#' @export
setMethod(
  "interpolate",
  signature(object = "Interpolation", x = "numeric"),
  function(object, x, ...) {
    object@func(x)
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

#' @export
interp_flatforward <- function() new("FlatForward", "flatforward")

#' @export
interp_linear <- function() new("Linear", "linear")

#' @export
interp_loglinear <- function() new("LogLinear", "loglinear")

#' @export
interp_naturalspline <- function() new("NaturalSpline", "naturalspline")

#' @export
interp_hermitespline <- function() new("HermiteSpline", "hermitespline")

#' @export
interp_monotonespline <- function() new("MonotoneSpline", "monotonespline")

#' @export
interp_nelsonsiegel <- function(beta1, beta2, beta3, lambda1) {
  new("NelsonSiegel", "nelsonsiegel",
    beta1 = beta1,
    beta2 = beta2,
    beta3 = beta3,
    lambda1 = lambda1
  )
}

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

#' @export
setGeneric(
  "parameters",
  function(x, ...) {
    standardGeneric("parameters")
  }
)

#' @export
setMethod(
  "parameters",
  signature(x = "NelsonSiegel"),
  function(x, ...) {
    c(beta1 = x@beta1, beta2 = x@beta2, beta3 = x@beta3, lambda1 = x@lambda1)
  }
)

#' @export
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