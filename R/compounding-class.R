
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
setGeneric(
  "compound",
  function(x, .t, .v, ...) {
    standardGeneric("compound")
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
  "compound",
  signature(x = "Simple", .t = "numeric", .v = "numeric"),
  function(x, .t, .v) (1 + .v*.t)
)

#' @export
setMethod(
  "rates",
  signature(x = "Simple", .t = "numeric", .v = "numeric"),
  function(x, .t, .v) (.v - 1)*(1/.t)
)

#' @export
setMethod(
  "compound",
  signature(x = "Discrete", .t = "numeric", .v = "numeric"),
  function(x, .t, .v) (1 + .v)^.t
)

#' @export
setMethod(
  "rates",
  signature(x = "Discrete", .t = "numeric", .v = "numeric"),
  function(x, .t, .v) .v^(1/.t) - 1
)

#' @export
setMethod(
  "compound",
  signature(x = "Continuous", .t = "numeric", .v = "numeric"),
  function(x, .t, .v) exp(.v*.t)
)

#' @export
setMethod(
  "rates",
  signature(x = "Continuous", .t = "numeric", .v = "numeric"),
  function(x, .t, .v) log(.v)*(1/.t)
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
  "compound",
  signature(x = "character", .t = "numeric", .v = "numeric"),
  function(x, .t, .v) {
    obj <- compounding(x)
    callGeneric(obj, .t, .v)
  }
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

#' @export
setMethod(
  "show",
  "Compounding",
  function(object) {
    cat("<compounding:", as(object, "character"), "\b>", "\n")
    invisible(object)
  }
)

# compound
# [character], term:[numeric], value
# [compounding], term:[numeric], value
# 

#' @export
setMethod(
  "Compare",
  signature(e1 = "Compounding", e2 = "Compounding"),
  function(e1, e2) {
    as(e1, "character") == as(e2, "character")
  }
)

