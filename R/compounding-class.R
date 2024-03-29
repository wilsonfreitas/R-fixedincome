#' Compounding class
#'
#' The Compounding class abstracts the compounding regime used to
#' discount or compound a spot rate.
#'
#' There are 3 compoundings:
#' \itemize{
#'   \item \code{simple} for simple interest rate compounding \deqn{1 + rt}
#'   \item \code{discrete} for compounded interest rate compounding
#'         \deqn{(1 + r)^t}
#'   \item \code{continuous} for continuous interest rate compounding
#'         \deqn{exp(rt)}
#' }
#'
#' The \code{Compounding} class has 2 methods:
#' \itemize{
#'   \item \code{compound} to compound the spot rate for a given term.
#'   \item \code{rates} to compute the implied rate for a compound factor
#'         in a given term.
#' }
#'
#' @aliases Simple-class Discrete-class Continuous-class Compounding-class
#' @aliases SimpleCompoundingClass
#' @aliases DiscreteCompoundingClass
#' @aliases ContinuousCompoundingClass
#'
#' @export
setClass(
  "Compounding",
  contains = "VIRTUAL"
)

#' @rdname Compounding
#' @export
SimpleCompoundingClass <- setClass(
  "Simple",
  contains = "Compounding"
)

#' @rdname Compounding
#' @export
DiscreteCompoundingClass <- setClass(
  "Discrete",
  contains = "Compounding"
)

#' @rdname Compounding
#' @export
ContinuousCompoundingClass <- setClass(
  "Continuous",
  contains = "Compounding"
)

#' Create Compounding class
#'
#' @description
#' `compound()` creates a `Compounding` object in one of its subclasses:
#' [Simple-class], [Discrete-class], [Continuous-class].
#'
#' @param x a character with the name of compounding regime:
#'        \code{simple}, \code{discrete}, \code{continuous}
#'
#' @details
#' A Compounding object can be instanciated with the \code{compounding}
#' function, passing a string with the name of one of the compounding regimes:
#' \code{simple}, \code{discrete}, \code{continuous}.
#'
#' @return A subclass of `Compounding` object.
#'
#' @examples
#' compounding("simple")
#' compounding("discrete")
#' compounding("continuous")
#'
#' comp <- compounding("discrete")
#' compound(comp, 0.06, 2) # equals (1 + 0.06) ^ 2 = 1.1236
#' implied_rate(comp, 1.1236, 2) # equals 0.06
#' @export
compounding <- function(x = c("simple", "discrete", "continuous")) {
  x <- match.arg(x)
  switch(x,
    "simple" = SimpleCompoundingClass(),
    "discrete" = DiscreteCompoundingClass(),
    "continuous" = ContinuousCompoundingClass()
  )
}

#' @export
as.character.Simple <- function(x, ...) "simple"

#' @export
as.character.Discrete <- function(x, ...) "discrete"

#' @export
as.character.Continuous <- function(x, ...) "continuous"

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

#' Implied rates
#'
#' Computes implied rates to compounding factors.
#'
#' @param x a Compounding object or a character with the compounding name.
#' @param t a numeric representing the term.
#' @param val a numeric representing the compounding factor.
#' @param ... additional arguments. Currently unused.
#'
#' @details
#' If the \code{x} argument is a \code{character} with a valid compounding name
#' (\code{simple}, \code{discrete}, \code{continuous}) the function
#' instanciates a Compounding object and then computes the implied rate
#' for the given compounding values and terms.
#'
#' @return A numeric value that represents a spot rate.
#'
#' @aliases
#' implied_rate,Continuous,numeric,numeric-method
#' implied_rate,Discrete,numeric,numeric-method
#' implied_rate,Simple,numeric,numeric-method
#' implied_rate,character,numeric,numeric-method
#'
#' @examples
#' implied_rate("simple", 2, 1.1)
#' implied_rate("discrete", 2, 1.1025)
#' implied_rate("continuous", 2, 1.105170918)
#'
#' comp <- compounding("discrete")
#' compound(comp, 0.06, 2) # equals (1 + 0.06) ^ 2 = 1.1236
#' implied_rate(comp, 1.1236, 2) # equals 0.06
#' @export
setGeneric(
  "implied_rate",
  function(x, t, val, ...) {
    standardGeneric("implied_rate")
  }
)

setMethod(
  "implied_rate",
  signature(x = "Simple", t = "numeric", val = "numeric"),
  function(x, t, val, ...) (val - 1) * (1 / t)
)

setMethod(
  "implied_rate",
  signature(x = "Discrete", t = "numeric", val = "numeric"),
  function(x, t, val, ...) val^(1 / t) - 1
)

setMethod(
  "implied_rate",
  signature(x = "Continuous", t = "numeric", val = "numeric"),
  function(x, t, val, ...) log(val) * (1 / t)
)

setMethod(
  "implied_rate",
  signature(x = "character", t = "numeric", val = "numeric"),
  function(x, t, val, ...) {
    obj <- compounding(x)
    callGeneric(obj, t, val)
  }
)
