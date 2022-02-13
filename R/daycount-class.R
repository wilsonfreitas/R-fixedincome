
# .daycounts.dib <- list(
#   '30/360' = 360,
#   '30E/360' = 360,
#   'actual/365' = 365,
#   'actual/360' = 360,
#   'business/252' = 252
# )

#' @export
setClass(
  "daycount",
  contains = "character"
)

#' @export
daycount <- function(spec, ...) {
  spec_parts = strsplit(spec, '/')[[1]]
  if (length(spec_parts) != 2)
    stop("Invalid daycount specitication provided (", spec, ")")
  new("daycount", .Data = spec)
}

setAs(
  "daycount", "character",
  def = function(from) from@.Data
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
    dc_parts <- unlist(strsplit(x@.Data, '/'))
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
  .ax <- data.frame(term_value, from_units, to_units, stringsAsFactors = FALSE)
  .rx <- sapply(split(.ax, seq_len(nrow(.ax))), function(x) {
    term_value <- x[, 1]
    from_units <- x[, 2]
    to_units <- x[, 3]
    if (from_units == 'day')
      term_value / dib
    else {
      r <- list(month=list(month=1, year=12), year=list(month=1/12, year=1))
      term_value * r[[ to_units ]][[ from_units ]]
    }
  })
  unname(.rx)
}

#' @export
setMethod(
  f = "timefactor",
  signature = c(x = "daycount", .t = "Term", .v = "missing"),
  def = function(x, .t, .v) {
    timefactor_(as(.t, "numeric"), units(.t), "year", dib(x))
  }
)

#' @export
setMethod(
  f = "timefactor",
  signature = c(x = "daycount", .t = "character", .v = "missing"),
  def = function(x, .t, .v) {
    tm <- as(.t, "Term")
    timefactor_(as(tm, "numeric"), units(tm), "year", dib(x))
  }
)

#' @export
setMethod(
  f = "timefactor",
  signature = c("daycount", "numeric", "character"),
  def = function(x, .t, .v) {
    tm <- term(.t, .v)
    timefactor_(as(tm, "numeric"), units(tm), "year", dib(x))
  }
)

#' @export
setMethod(
  "show",
  "daycount",
  function(object) {
    cat("<daycount:", object@.Data, "\b>", "\n")
    invisible(object)
  }
)

