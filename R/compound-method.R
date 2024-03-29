#' Compound method
#'
#' Computes the compounding (and discount) factor for spot rates and curves.
#'
#' @param x can be a \code{Compounding}, a \code{SpotRate},
#'        a \code{SpotRateCurve}, a \code{ForwardRate} and a character
#'        representing a \code{Compounding}.
#' @param t represents the term to compound. Can be a numeric, a \code{Term},
#'        a \code{Date} or even missing. See Details.
#' @param val is the value of the spot rate to be compounded in the given term.
#'        Can be a numeric, a \code{Date} or missing. See Details.
#' @param ... additional arguments.
#'
#' @details
#' For \code{Compounding} classes the arguments \code{t} and \code{val} must be
#' provided.
#'
#' For a \code{SpotRate} class, if the \code{t} argument is numeric,
#' representing the term to be compounded, the argument \code{val} must
#' be a character with the units of the Term class. If otherwise \code{t}
#' is a Term object, \code{val} is missing.
#'
#' For \code{SpotRateCurve} and \code{ForwardRate} classes, that already have
#' terms associated, \code{t} and \code{val} are missing.
#'
#' `discount()` method is the inverse of compound: `1 / compound()`.
#'
#' @return A numeric value that represents the compounding factor for the given
#'         spot rate.
#'
#' @aliases
#' compound,Continuous,numeric,numeric-method
#' compound,Discrete,numeric,numeric-method
#' compound,ForwardRate,missing,missing-method
#' compound,Simple,numeric,numeric-method
#' compound,SpotRate,Date,Date-method
#' compound,SpotRate,Term,missing-method
#' compound,SpotRate,numeric,character-method
#' compound,SpotRateCurve,missing,missing-method
#' compound,character,numeric,numeric-method
#' discount,ForwardRate,missing,missing-method
#' discount,SpotRate,Date,Date-method
#' discount,SpotRate,Term,missing-method
#' discount,SpotRate,numeric,character-method
#' discount,SpotRateCurve,missing,missing-method
#'
#' @examples
#' compound("simple", 2, 0.05)
#' compound("discrete", 2, 0.05)
#' compound("continuous", 2, 0.05)
#'
#' spr <- spotrate(0.06, "simple", "actual/365", "actual")
#' compound(spr, 10, "days")
#' discount(spr, 10, "days")
#' t <- term(10, "days")
#' compound(spr, t)
#' discount(spr, t)
#' d1 <- Sys.Date()
#' d2 <- Sys.Date() + 10
#' compound(spr, d1, d2)
#' discount(spr, d1, d2)
#'
#' terms <- c(1, 11, 26, 27, 28)
#' rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
#' curve <- spotratecurve(rates, terms, "discrete", "actual/365", "actual")
#' compound(curve)
#' discount(curve)
#' @export
setGeneric(
  "compound",
  function(x, t, val, ...) {
    standardGeneric("compound")
  }
)

setMethod(
  "compound",
  signature(x = "Simple", t = "numeric", val = "numeric"),
  function(x, t, val) (1 + val * t)
)

setMethod(
  "compound",
  signature(x = "Discrete", t = "numeric", val = "numeric"),
  function(x, t, val) (1 + val)^t
)

setMethod(
  "compound",
  signature(x = "Continuous", t = "numeric", val = "numeric"),
  function(x, t, val) exp(val * t)
)

setMethod(
  "compound",
  signature(x = "character", t = "numeric", val = "numeric"),
  function(x, t, val) {
    obj <- compounding(x)
    callGeneric(obj, t, val)
  }
)

setMethod(
  "compound",
  signature(x = "SpotRate", t = "numeric", val = "character"),
  function(x, t, val = "days") {
    tm <- term(t, val)
    tf <- as.numeric(toyears(x@daycount, tm))
    callGeneric(x@compounding, tf, x@.Data)
  }
)

setMethod(
  "compound",
  signature(x = "SpotRate", t = "Term", val = "missing"),
  function(x, t, val) {
    tf <- as.numeric(toyears(x@daycount, t))
    callGeneric(x@compounding, tf, x@.Data)
  }
)

setMethod(
  "compound",
  signature(x = "SpotRate", t = "Date", val = "Date"),
  function(x, t, val) {
    tm <- term(t, val, x@calendar)
    tf <- as.numeric(toyears(x@daycount, tm))
    callGeneric(x@compounding, tf, x@.Data)
  }
)

setMethod(
  "compound",
  signature(x = "SpotRateCurve", t = "missing", val = "missing"),
  function(x, t, val) {
    callGeneric(x, x@terms)
  }
)

setMethod(
  "compound",
  signature(x = "ForwardRate", t = "missing", val = "missing"),
  function(x, t, val) {
    callGeneric(x, x@terms)
  }
)

#' @rdname compound
#' @export
setGeneric(
  "discount",
  function(x, t, val, ...) {
    standardGeneric("discount")
  }
)

setMethod(
  "discount",
  signature(x = "SpotRate", t = "numeric", val = "character"),
  function(x, t, val) {
    1 / compound(x, t, val)
  }
)

setMethod(
  "discount",
  signature(x = "SpotRate", t = "Term", val = "missing"),
  function(x, t, val) {
    1 / compound(x, t)
  }
)

setMethod(
  "discount",
  signature(x = "SpotRate", t = "Date", val = "Date"),
  function(x, t, val) {
    1 / compound(x, t, val)
  }
)

setMethod(
  "discount",
  signature(x = "SpotRateCurve", t = "missing", val = "missing"),
  function(x, t, val) {
    callGeneric(x, x@terms)
  }
)

setMethod(
  "discount",
  signature(x = "ForwardRate", t = "missing", val = "missing"),
  function(x, t, val) {
    callGeneric(x, x@terms)
  }
)