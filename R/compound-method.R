
#' Compound method
#' 
#' Computes the compounding factor for spot rates.
#' 
#' @param x can be a \code{Compounding}, a \code{SpotRate}, a \code{SpotRateCurve},
#'          a \code{ForwardRate} and a character representing a \code{Compounding}.
#' @param t represents the term to compound. Can be a numeric, a \code{Term},
#'        a \code{Date} or even missing. See Details.
#' @param val is the value of the spot rate to be compounded in the given term.
#'        Can be a numeric, a \code{Date} or missing. See Details.
#' 
#' @details
#' For \code{Compounding} classes the arguments \code{t} and \code{val} must be
#' provided.
#' 
#' For a \code{SpotRate} class, if the \code{t} argument is numeric, representing
#' the term to be compounded, the argument \code{val} must be a character with
#' the units of the Term class. If otherwise \code{t} is a Term object, \code{val}
#' is missing.
#' 
#' For \code{SpotRateCurve} and \code{ForwardRate} classes, that already have
#' terms associated, \code{t} and \code{val} are missing.
#' 
#' @return a numeric value that represents the compounding factor for the given
#'         spot rate.
#' 
#' @name compound-method
#' @examples
#' 
#' compound("simple", 2, 0.05)
#' compound("discrete", 2, 0.05)
#' compound("continuous", 2, 0.05)
#' 
#' spr <- spotrate(0.06, "simple", "actual/365")
#' compound(spr, 10, "days")
#' t <- term(10, "days")
#' compound(spr, t)
#' d1 <- Sys.Date()
#' d2 <- Sys.Date() + 10
#' compound(spr, d1, d2)
#' 
#' terms <- c(1, 11, 26, 27, 28)
#' rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
#' curve <- spotratecurve(rates, terms, "discrete", "actual/365", "actual")
#' compound(curve)
#' 
NULL

#' @rdname compound-method
#' @export
setGeneric(
  "compound",
  function(x, t, val, ...) {
    standardGeneric("compound")
  }
)

#' @rdname compound-method
#' @export
setMethod(
  "compound",
  signature(x = "Simple", t = "numeric", val = "numeric"),
  function(x, t, val) (1 + val*t)
)

#' @rdname compound-method
#' @export
setMethod(
  "compound",
  signature(x = "Discrete", t = "numeric", val = "numeric"),
  function(x, t, val) (1 + val)^t
)

#' @rdname compound-method
#' @export
setMethod(
  "compound",
  signature(x = "Continuous", t = "numeric", val = "numeric"),
  function(x, t, val) exp(val*t)
)

#' @rdname compound-method
#' @export
setMethod(
  "compound",
  signature(x = "character", t = "numeric", val = "numeric"),
  function(x, t, val) {
    obj <- compounding(x)
    callGeneric(obj, t, val)
  }
)

#' @rdname compound-method
#' @export
setMethod(
  "compound",
  signature(x = "SpotRate", t = "numeric", val = "character"),
  function(x, t, val = "days") {
    tm <- term(t, val)
    tf <- timefactor(x@daycount, tm)
    compound(x@compounding, tf, x@.Data)
  }
)

#' @rdname compound-method
#' @export
setMethod(
  "compound",
  signature(x = "SpotRate", t = "Term", val = "missing"),
  function(x, t, val) {
    tf <- timefactor(x@daycount, t)
    compound(x@compounding, tf, x@.Data)
  }
)

#' @rdname compound-method
#' @export
setMethod(
  "compound",
  signature(x = "SpotRate", t = "Date", val = "Date"),
  function(x, t, val) {
    tm <- term(t, val, x@calendar)
    tf <- timefactor(x@daycount, tm)
    compound(x@compounding, tf, x@.Data)
  }
)

#' @rdname compound-method
#' @export
setMethod(
  "compound",
  signature(x = "SpotRateCurve", t = "missing", val = "missing"),
  function(x, t, val) {
    compound(x, x@terms)
  }
)

#' @rdname compound-method
#' @export
setMethod(
  "compound",
  signature(x = "ForwardRate", t = "missing", val = "missing"),
  function(x, t, val) {
    compound(x, x@terms)
  }
)

