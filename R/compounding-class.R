
#' @export
CompoundingClass <- setClass(
  "compounding",
  contains = "VIRTUAL"
)

#' @export
SimpleCompoundingClass <- setClass(
  "simple",
  contains = "compounding"
)

#' @export
DiscreteCompoundingClass <- setClass(
  "discrete",
  contains = "compounding"
)

#' @export
ContinuousCompoundingClass <- setClass(
  "continuous",
  contains = "compounding"
)

setAs(
  "simple", "character",
  def = function(from) "simple"
)

setAs(
  "discrete", "character",
  def = function(from) "discrete"
)

setAs(
  "continuous", "character",
  def = function(from) "continuous"
)

#' @export
setGeneric(
  name = "compound",
  def = function(x, .t, .v, ...) {
    standardGeneric("compound")
  }
)

#' @export
setGeneric(
  name = "rates",
  def = function(x, .t, .v, ...) {
    standardGeneric("rates")
  }
)

#' @export
setMethod(
  f = "compound",
  signature = c("simple", "numeric", "numeric"),
  def = function(x, .t, .v) (1 + .v*.t)
)

#' @export
setMethod(
  f = "rates",
  signature = c("simple", "numeric", "numeric"),
  def = function(x, .t, .v) (.v - 1)*(1/.t)
)

#' @export
setMethod(
  f = "compound",
  signature = c("discrete", "numeric", "numeric"),
  def = function(x, .t, .v) (1 + .v)^.t
)

#' @export
setMethod(
  f = "rates",
  signature = c("discrete", "numeric", "numeric"),
  def = function(x, .t, .v) .v^(1/.t) - 1
)

#' @export
setMethod(
  f = "compound",
  signature = c("continuous", "numeric", "numeric"),
  def = function(x, .t, .v) exp(.v*.t)
)

#' @export
setMethod(
  f = "rates",
  signature = c("continuous", "numeric", "numeric"),
  def = function(x, .t, .v) log(.v)*(1/.t)
)

#' @export
compounding <- function(x = c("simple", "discrete", "continuous")) {
  x <- match.arg(x)
  switch(x,
         "simple" = SimpleCompoundingClass(),
         "discrete" = DiscreteCompoundingClass(),
         "continuous" = ContinuousCompoundingClass())
}

#' @export
setMethod(
  f = "compound",
  signature = c("character", "numeric", "numeric"),
  def = function(x, .t, .v) {
    obj <- compounding(x)
    callGeneric(obj, .t, .v)
  }
)

#' @export
setMethod(
  f = "rates",
  signature = c("character", "numeric", "numeric"),
  def = function(x, .t, .v) {
    obj <- compounding(x)
    callGeneric(obj, .t, .v)
  }
)

# compound
# [character], term:[numeric], value
# [compounding], term:[numeric], value
# 
