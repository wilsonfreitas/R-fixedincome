
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
setMethod(
  "interpolation",
  signature(x = "character"),
  function(x) {
    switch(x,
           "flatforward" = new("FlatForward", "flatforward"),
           "linear" = new("Linear", "linear"))
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
