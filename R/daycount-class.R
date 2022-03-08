#' Daycount class
#'
#' @description
#' \code{Daycount} class helps computing terms used to compound interest rates.
#'
#' @details
#' The daycount convention determines the amount of days, in years, represents
#' the given term.
#'
#' \code{daycount} creates a \code{Daycount} object and accepts the
#' following daycount rules: \code{actual/365}, \code{actual/360},
#' \code{business/252}
#'
#' The method \code{dib} returns the days in base for a daycount convention.
#' Since we work with annual rates the days in base define the amount of days
#' in a year used in the convention.
#'
#' \code{toyears} returns the given term in years, since we are assuming
#' annual rates.
#' The \code{t} argument can be a term instance, a string defining a term
#' or a numeric.
#' In the last alternative, the \code{units} argument must be
#' provided with a valid Term units (days, months, years).
#'
#' @param x a character representing a daycount rule, like: \code{business/252},
#'        \code{actual/365}, \code{actual/360}, ...
#' @param t represents the term to compound. Can be a numeric, a \code{Term},
#'        or a character representing a \code{Term}. See Details.
#' @param units a character with the Term units. Can also be missing.
#'        See Details.
#' @param ... additional arguments
#'
#' @aliases Daycount-class
#'
#' @name daycount-class
#' @examples
#' dc <- daycount("actual/360")
#' dib(dc)
#'
#' toyears(dc, 10, "days")
#' t <- term(10, "months")
#' toyears(dc, t)
NULL

#' @rdname daycount-class
#' @export
daycount <- function(x, ...) {
  spec_parts <- strsplit(x, "/")[[1]]
  if (length(spec_parts) != 2) {
    stop("Invalid daycount specitication provided (", x, ")")
  }
  new("Daycount", .Data = x)
}

#' @export
setClass(
  "Daycount",
  contains = "character"
)

as.character.Daycount <- function(from) from@.Data

#' @export
setMethod(
  "show",
  signature(object = "Daycount"),
  function(object) {
    cat("<daycount:", object@.Data, "\b>", "\n")
    invisible(object)
  }
)

#' @export
setGeneric(
  "dib",
  function(x) {
    standardGeneric("dib")
  }
)

#' @rdname daycount-class
#' @export
setMethod(
  "dib",
  signature(x = "Daycount"),
  function(x) {
    dc_parts <- unlist(strsplit(x@.Data, "/"))
    as.numeric(dc_parts[2])
  }
)

#' @export
setGeneric(
  "toyears",
  function(x, t, units) {
    standardGeneric("toyears")
  }
)

ym_conv_map <- list(
  month = list(month = 1, year = 12),
  year = list(month = 1 / 12, year = 1)
)

convert_term_value <- function(x, dib) {
  term_value <- x[, 1]
  from_units <- x[, 2]
  to_units <- x[, 3]
  if (from_units == "day") {
    term_value / dib
  } else {
    term_value * ym_conv_map[[to_units]][[from_units]]
  }
}

toyears_ <- function(term_value, from_units, to_units, dib) {
  ax <- data.frame(term_value, from_units, to_units, stringsAsFactors = FALSE)
  rx <- sapply(split(ax, seq_len(nrow(ax))), convert_term_value, dib = dib)
  unname(rx)
}

#' @rdname daycount-class
#' @export
setMethod(
  "toyears",
  signature(x = "Daycount", t = "Term", units = "missing"),
  function(x, t, units) {
    toyears_(as(t, "numeric"), t@units, "year", dib(x))
  }
)

#' @rdname daycount-class
#' @export
setMethod(
  "toyears",
  signature(x = "Daycount", t = "character", units = "missing"),
  function(x, t, units) {
    tm <- as.term(t)
    toyears_(as(tm, "numeric"), tm@units, "year", dib(x))
  }
)

#' @rdname daycount-class
#' @export
setMethod(
  "toyears",
  signature(x = "Daycount", t = "numeric", units = "character"),
  function(x, t, units) {
    tm <- term(t, units)
    toyears_(as(tm, "numeric"), tm@units, "year", dib(x))
  }
)