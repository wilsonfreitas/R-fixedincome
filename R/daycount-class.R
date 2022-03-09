#' Daycount class
#'
#' `Daycount` class helps adjusting the terms to compound interest rates.
#' With annual rates it is necessary to convert periods of days or months
#' to years units.
#' The day count convention helps with that by defining the number of days of
#' one year.
#' Together with a calendar it defines the way the wordays are counted between
#' two dates.
#'
#' Common day count rules are: \code{actual/365}, \code{actual/360},
#' `business/252`, `30/360`, ...
#'
#' @export
setClass(
  "Daycount",
  contains = "character"
)

#' Create Daycount class
#'
#' \code{daycount} creates a \code{Daycount} object.
#' It accepts the following daycount rules: \code{actual/365},
#' \code{actual/360}, \code{business/252}.
#'
#' @param x a character representing a daycount rule, like: \code{business/252},
#'        \code{actual/365}, \code{actual/360}, ...
#' @param ... additional arguments. Currently unused.
#'
#' @return A Daycount object.
#'
#' @examples
#' dc <- daycount("actual/360")
#' @export
daycount <- function(x, ...) {
  spec_parts <- strsplit(x, "/")[[1]]
  if (length(spec_parts) != 2) {
    stop("Invalid daycount specitication provided (", x, ")")
  }
  new("Daycount", .Data = x)
}

#' @export
as.character.Daycount <- function(x, ...) x@.Data

#' @export
setMethod(
  "show",
  signature(object = "Daycount"),
  function(object) {
    cat("<daycount:", object@.Data, "\b>", "\n")
    invisible(object)
  }
)

#' Days in base for Daycount
#'
#' @description
#' \code{dib} returns the days in base, that is the number of days used to
#' define one year.
#'
#' @details
#' The method \code{dib} returns the days in base for a daycount convention.
#' Since we work with annual rates the days in base define the amount of days
#' in a year used in the convention.
#'
#' @param x a Daycount object.
#'
#' @aliases dib,Daycount-method
#'
#' @return A numeric.
#'
#' @examples
#' dc <- daycount("actual/360")
#' dib(dc)
#' @export
setGeneric(
  "dib",
  function(x) {
    standardGeneric("dib")
  }
)

setMethod(
  "dib",
  signature(x = "Daycount"),
  function(x) {
    dc_parts <- unlist(strsplit(x@.Data, "/"))
    as.numeric(dc_parts[2])
  }
)

#' Terms in years according to Daycount
#'
#' @description
#' \code{toyears} returns a numeric representing a Term in years.
#'
#' @details
#' \code{toyears} returns the given term in years, since we are assuming
#' annual rates.
#' The \code{t} argument can be a term instance, a string defining a term
#' or a numeric.
#' In the last alternative, the \code{units} argument must be
#' provided with a valid Term units (days, months, years).
#'
#' @param x a Daycount object.
#' @param t represents the term to compound. Can be a numeric, a \code{Term},
#'        or a character representing a \code{Term}. See Details.
#' @param units a character with the Term units. Can also be missing.
#'        See Details.
#'
#' @aliases
#' toyears,Daycount,Term,missing-method
#' toyears,Daycount,character,missing-method
#' toyears,Daycount,numeric,character-method
#'
#' @return A numeric.
#'
#' @examples
#' dc <- daycount("actual/360")
#' toyears(dc, 10, "days")
#' t <- term(10, "months")
#' toyears(dc, t)
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

setMethod(
  "toyears",
  signature(x = "Daycount", t = "Term", units = "missing"),
  function(x, t, units) {
    toyears_(as(t, "numeric"), t@units, "year", dib(x))
  }
)

setMethod(
  "toyears",
  signature(x = "Daycount", t = "character", units = "missing"),
  function(x, t, units) {
    tm <- as.term(t)
    toyears_(as(tm, "numeric"), tm@units, "year", dib(x))
  }
)

setMethod(
  "toyears",
  signature(x = "Daycount", t = "numeric", units = "character"),
  function(x, t, units) {
    tm <- term(t, units)
    toyears_(as(tm, "numeric"), tm@units, "year", dib(x))
  }
)