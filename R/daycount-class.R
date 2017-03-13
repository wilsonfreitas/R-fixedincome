
# .daycounts.dib <- list(
#   '30/360' = 360,
#   '30E/360' = 360,
#   'actual/365' = 365,
#   'actual/360' = 360,
#   'business/252' = 252
# )

#' @export
daycount <- setClass(
  "daycount",
  representation = representation(spec = "character")
)

#' @export daycount
setMethod(
  "initialize",
  "daycount",
  function(.Object, spec) {
    .Object@spec <- spec
    .Object
  }
)

setAs(
  "daycount", "character",
  def = function(from) from@spec
)

#' @export
setGeneric(
  name = "dib",
  def = function(x) {
    standardGeneric("dib")
  }
)

#' @export
setMethod(
  f = "dib",
  signature = c("daycount"),
  def = function(x) {
    dc_parts <- unlist(strsplit(x@spec, '/'))
    as.numeric(dc_parts[2])
  }
)

#' @export
setGeneric(
  name = "timefactor",
  def = function(x, .t, .v) {
    standardGeneric("timefactor")
  }
)

timefactor_ <- function(term_value, from_units, to_units, dib) {
  if (from_units == 'days')
    term_value / dib
  else {
    r <- list(months=list(months=1, years=12), years=list(months=1/12, years=1))
    term_value * r[[ to_units ]][[ from_units ]]
  }
}

#' @export
setMethod(
  f = "timefactor",
  signature = c(x = "daycount", .t = "term", .v = "missing"),
  def = function(x, .t, .v) {
    timefactor_(as(.t, "numeric"), units(.t), "years", dib(x))
  }
)

#' @export
setMethod(
  f = "timefactor",
  signature = c(x = "daycount", .t = "character", .v = "missing"),
  def = function(x, .t, .v) {
    tm <- as(.t, "term")
    timefactor_(as(tm, "numeric"), units(tm), "years", dib(x))
  }
)

#' @export
setMethod(
  f = "timefactor",
  signature = c("daycount", "numeric", "character"),
  def = function(x, .t, .v) {
    tm <- term(.t, .v)
    timefactor_(as(tm, "numeric"), units(tm), "years", dib(x))
  }
)
