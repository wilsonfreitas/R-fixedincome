

#' @export
setGeneric(
  "compound",
  function(x, .t, .v, ...) {
    standardGeneric("compound")
  }
)

#' @export
setMethod(
  "compound",
  signature(x = "Simple", .t = "numeric", .v = "numeric"),
  function(x, .t, .v) (1 + .v*.t)
)

#' @export
setMethod(
  "compound",
  signature(x = "Discrete", .t = "numeric", .v = "numeric"),
  function(x, .t, .v) (1 + .v)^.t
)

#' @export
setMethod(
  "compound",
  signature(x = "Continuous", .t = "numeric", .v = "numeric"),
  function(x, .t, .v) exp(.v*.t)
)

#' @export
setMethod(
  "compound",
  signature(x = "character", .t = "numeric", .v = "numeric"),
  function(x, .t, .v) {
    obj <- compounding(x)
    callGeneric(obj, .t, .v)
  }
)

#' @export
setMethod(
  "compound",
  signature(x = "SpotRate", .t = "numeric", .v = "character"),
  function(x, .t, .v = "days") {
    tm <- term(.t, .v)
    tf <- timefactor(x@daycount, tm)
    compound(x@compounding, tf, x@.Data)
  }
)

#' @export
setMethod(
  "compound",
  signature(x = "SpotRate", .t = "Term", .v = "missing"),
  function(x, .t, .v) {
    tf <- timefactor(x@daycount, .t)
    compound(x@compounding, tf, x@.Data)
  }
)

#' @export
setMethod(
  "compound",
  signature(x = "SpotRate", .t = "Date", .v = "Date"),
  function(x, .t, .v) {
    tm <- term(.t, .v, x@calendar)
    tf <- timefactor(x@daycount, tm)
    compound(x@compounding, tf, x@.Data)
  }
)

#' @export
setMethod(
  "compound",
  signature(x = "SpotRateCurve", .t = "missing", .v = "missing"),
  function(x, .t, .v) {
    compound(x, x@terms)
  }
)

#' @export
setMethod(
  "compound",
  signature(x = "ForwardRate", .t = "missing", .v = "missing"),
  function(x, .t, .v) {
    compound(x, x@terms)
  }
)

