#' Basis points
#'
#' A basis point (bp) is 0.01%, or 0.0001.
#' `bps()` creates a `Bps` object from a number of basis points, and
#' `as.bps()` converts a rate difference, given as a decimal, to basis points.
#' `Bps` extends `numeric` and keeps the value as a decimal, so
#' `as.numeric(bps(10))` is `0.001`.
#'
#' Adding or subtracting basis points to a `SpotRate` (or a `SpotRateCurve`)
#' returns the same object with the rates shifted; compounding, daycount,
#' calendar, terms and interpolation are kept.
#' Two `Bps` objects can be added or subtracted, and divided (returning a
#' numeric). `c()` combines `Bps` objects.
#'
#' @param x a numeric with the number of basis points (`bps()`), or a rate
#'   difference as a decimal, like a `SpotRate` spread (`as.bps()`).
#'
#' @return a `Bps` object.
#'
#' @examples
#' bps(10)
#' as.numeric(bps(10))
#'
#' sr <- spotrate(0.1375, "discrete", "business/252", "Brazil/ANBIMA")
#' sr + bps(10)
#' sr - bps(c(10, 25))
#'
#' sr2 <- spotrate(0.1325, "discrete", "business/252", "Brazil/ANBIMA")
#' as.bps(sr - sr2)
#'
#' @aliases Bps-class
#' @export
bps <- function(x) {
  new("Bps", as.numeric(x) / 1e4)
}

#' @rdname bps
#' @export
as.bps <- function(x) {
  new("Bps", as.numeric(x))
}

setClass("Bps", contains = "numeric")

#' @export
format.Bps <- function(x, ...) {
  paste(format(x@.Data * 1e4, trim = TRUE, ...), "bps")
}

setMethod("show", "Bps", function(object) print(format(object)))

# Plain numbers are rejected: it is not clear if they are bps or decimals.
#' @export
c.Bps <- function(x, ...) {
  elements <- c(list(x), list(...))
  if (!all(vapply(elements, is, logical(1), "Bps"))) {
    stop("Only Bps objects can be combined with Bps: use bps() first")
  }
  new("Bps", unlist(lapply(elements, as.numeric)))
}

#' @export
setMethod(
  "[",
  signature(x = "Bps"),
  function(x, i, ...) new("Bps", x@.Data[i])
)

#' @export
setMethod(
  "Arith",
  signature(e1 = "Bps", e2 = "Bps"),
  function(e1, e2) {
    value <- callGeneric(e1@.Data, e2@.Data)
    switch(.Generic,
      "+" = ,
      "-" = new("Bps", value),
      "/" = value,
      stop("Operation ", .Generic, " is not defined for two Bps")
    )
  }
)

#' @export
setMethod(
  "Arith",
  signature(e1 = "SpotRate", e2 = "Bps"),
  function(e1, e2) {
    if (!.Generic %in% c("+", "-")) {
      stop("Only + and - are defined between SpotRate and Bps")
    }
    e1@.Data <- callGeneric(e1@.Data, e2@.Data)
    refresh_interpolation(e1)
  }
)

#' @export
setMethod(
  "Arith",
  signature(e1 = "Bps", e2 = "SpotRate"),
  function(e1, e2) {
    if (.Generic != "+") {
      stop("Only + is defined between Bps and SpotRate")
    }
    e2@.Data <- e1@.Data + e2@.Data
    refresh_interpolation(e2)
  }
)
