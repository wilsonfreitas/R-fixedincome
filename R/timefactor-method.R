
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
  "timefactor",
  signature(x = "Daycount", .t = "Term", .v = "missing"),
  function(x, .t, .v) {
    timefactor_(as(.t, "numeric"), .t@units, "year", dib(x))
  }
)

#' @export
setMethod(
  "timefactor",
  signature(x = "Daycount", .t = "character", .v = "missing"),
  function(x, .t, .v) {
    tm <- as.term(.t)
    timefactor_(as(tm, "numeric"), tm@units, "year", dib(x))
  }
)

#' @export
setMethod(
  "timefactor",
  signature(x = "Daycount", .t = "numeric", .v = "character"),
  function(x, .t, .v) {
    tm <- term(.t, .v)
    timefactor_(as(tm, "numeric"), tm@units, "year", dib(x))
  }
)
