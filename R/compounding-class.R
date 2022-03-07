#' Compounding class
#' 
#' @description
#' The Compounding class abstracts the compounding regime used to 
#' discount or compound a spot rate.
#' 
#' @param x a character with the name of compounding regime:
#'        \code{simple}, \code{discrete}, \code{continuous}
#' 
#' @details
#' There are 3 compoundings:
#' \itemize{
#'   \item \code{simple} for simple interest rate compounding \deqn{1 + rt}
#'   \item \code{discrete} for compounded interest rate compounding \deqn{(1 + r)^t}
#'   \item \code{continuous} for continuous interest rate compounding \deqn{exp(rt)}
#' }
#' 
#' A Compounding object can be instanciated with the \code{compounding} function,
#' passing a string with the name of one of the compounding regimes:
#' \code{simple}, \code{discrete}, \code{continuous}.
#' 
#' The \code{compounding} class has 2 methods:
#' \itemize{
#'   \item \code{compound} to compound the spot rate for a given term.
#'   \item \code{rates} to compute the implied rate for a compound factor in a given term.
#' }
#' 
#' @name compounding-class
#' @examples
#' compounding("simple")
#' compounding("discrete")
#' compounding("continuous")
#' 
#' comp <- compounding("discrete")
#' compound(comp, 0.06, 2) # equals (1 + 0.06) ^ 2 = 1.1236
#' rates(comp, 1.1236, 2) # equals 0.06
NULL

#' @name compounding-class
#' @export
compounding <- function(x = c("simple", "discrete", "continuous")) {
  x <- match.arg(x)
  switch(x,
         "simple" = SimpleCompoundingClass(),
         "discrete" = DiscreteCompoundingClass(),
         "continuous" = ContinuousCompoundingClass())
}

#' @export
CompoundingClass <- setClass(
  "Compounding",
  contains = "VIRTUAL"
)

#' @export
SimpleCompoundingClass <- setClass(
  "Simple",
  contains = "Compounding"
)

#' @export
DiscreteCompoundingClass <- setClass(
  "Discrete",
  contains = "Compounding"
)

#' @export
ContinuousCompoundingClass <- setClass(
  "Continuous",
  contains = "Compounding"
)

setAs(
  "Simple",
  "character",
  function(from) "simple"
)

setAs(
  "Discrete",
  "character",
  function(from) "discrete"
)

setAs(
  "Continuous",
  "character",
  function(from) "continuous"
)

#' @export
setMethod(
  "show",
  signature(object = "Compounding"),
  function(object) {
    cat("<compounding:", as(object, "character"), "\b>", "\n")
    invisible(object)
  }
)

#' @export
setMethod(
  "==",
  signature(e1 = "Compounding", e2 = "Compounding"),
  function(e1, e2) {
    as(e1, "character") == as(e2, "character")
  }
)

#' @export
setMethod(
  "!=",
  signature(e1 = "Compounding", e2 = "Compounding"),
  function(e1, e2) {
    as(e1, "character") != as(e2, "character")
  }
)

#' @export
setGeneric(
  "rates",
  function(x, .t, .v, ...) {
    standardGeneric("rates")
  }
)

#' @export
setMethod(
  "rates",
  signature(x = "Simple", .t = "numeric", .v = "numeric"),
  function(x, .t, .v) (.v - 1)*(1/.t)
)

#' @export
setMethod(
  "rates",
  signature(x = "Discrete", .t = "numeric", .v = "numeric"),
  function(x, .t, .v) .v^(1/.t) - 1
)

#' @export
setMethod(
  "rates",
  signature(x = "Continuous", .t = "numeric", .v = "numeric"),
  function(x, .t, .v) log(.v)*(1/.t)
)

#' @export
setMethod(
  "rates",
  signature(x = "character", .t = "numeric", .v = "numeric"),
  function(x, .t, .v) {
    obj <- compounding(x)
    callGeneric(obj, .t, .v)
  }
)
