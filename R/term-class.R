
#' Create Term class
#'
#' `term()` creates a Term object.
#'
#' @param x can be a numeric value representing the time period, a Term object,
#' or the initial date for a period between two dates.
#' @param units one of the valid \code{units}: \code{days}, \code{monts},
#' \code{years}.
#' @param end_date the final date for a period between two dates.
#' @param calendar the calendar used to compute the amount of days for a period
#' between two dates.
#' @param ... additional arguments
#'
#' @examples
#' term(6, "months")
#' if (require("bizdays")) {
#'   term(as.Date("2022-02-02"), as.Date("2022-02-23"), "Brazil/ANBIMA")
#' }
#' @export
term <- function(x, ...) {
  UseMethod("term")
}

#' @rdname term
#' @export
term.numeric <- function(x, units = "days", ...) {
  value <- x

  if (length(units) > 1) {
    warning("units length > 1 and only the first element will be used")
    units <- units[1]
  }

  units <- sub("^(.*)s$", "\\1", units)
  stopifnot(units %in% c("year", "month", "day"))

  new("Term", .Data = value, units = units)
}

#' @rdname term
#' @export
term.Term <- function(x, ...) {
  x
}

#' @rdname term
#' @export
term.Date <- function(x, end_date, calendar, ...) {
  start_date <- x
  new("DateRangeTerm", bizdays(start_date, end_date, calendar),
    start_date = start_date, end_date = end_date, calendar = calendar,
    units = "day"
  )
}

#' Term class
#'
#' It is the time interval used in calculations with interest rates.
#' The term class represents the period used to discount or compound a spot
#' rate.
#' It can be Term object or a DateRangeTerm which defines start and end dates
#' and a calendar to count the amount of working days between these two dates.
#'
#' @aliases DateRangeTerm-class
#' @export
setClass(
  "Term",
  slots = c(units = "character"),
  contains = "numeric"
)

#' @export
setClass(
  "DateRangeTerm",
  slots = c(start_date = "Date", end_date = "Date", calendar = "character"),
  contains = c("Term", "numeric")
)

#' @export
setMethod(
  "show",
  signature("Term"),
  function(object) {
    print(format(object))
  }
)

#' Coerce a character to a Term
#'
#' `as.term` coerces a character vector to a Term object.
#'
#' @param x a character to be coerced to a Term.
#' @param ... additional arguments. Currently unused.
#'
#' @return A Term object.
#'
#' @details
#' The string representation of the Term class follows the layout:
#'
#' \preformatted{NUMBER UNITS}
#'
#' where units is one of: days, months, years.
#'
#' @aliases as.term,character-method
#' @examples
#' t <- as.term("6 months")
#' @export
setGeneric(
  "as.term",
  function(x, ...) {
    standardGeneric("as.term")
  }
)

setMethod(
  "as.term",
  signature(x = "character"),
  function(x, ...) {
    m <- regexec(
      "^([0-9]+)(\\.[0-9]+)? (years|months|days|year|month|day)?$",
      x
    )
    m <- unlist(regmatches(x, m))
    if (length(m)) {
      term(as.numeric(paste0(m[2], m[3])), m[4])
    } else {
      stop("Invalid term: ", x)
    }
  }
)

#' @export
as.character.Term <- function(x, ...) {
  format(x)
}

setMethod(
  "shift",
  signature(x = "Term"),
  function(x, k = 1, ..., fill = NA) {
    shifted <- shift(as.numeric(x), k, fill = fill)
    term(shifted, x@units)
  }
)

#' Calculate lagged differences of Term objects
#'
#' \code{diff} returns a Term vector with lagged differences.
#'
#' @param x a Term object.
#' @param lag a numerix indicating which lag to use.
#' @param fill a numeric value (or \code{NA}) to fill the empty created by
#' applying diff to a Term object.
#' @param ... additional arguments. Currently unused.
#'
#' @return A Term object.
#' @examples
#' t <- term(1:10, "months")
#' diff(t)
#' @export
setMethod(
  "diff",
  signature(x = "Term"),
  function(x, lag = 1, ..., fill = NULL) {
    diff_x <- as.numeric(diff(x@.Data, lag = lag))
    if (is.null(fill)) {
      term(diff_x, x@units)
    } else {
      fill <- as.numeric(fill)
      term(c(fill, diff_x), x@units)
    }
  }
)

#' @export
format.Term <- function(x, ...) {
  value <- x@.Data
  abrev <- x@units
  abrev <- ifelse(value > 1, paste0(abrev, "s"), abrev)
  paste(value, abrev, sep = " ")
}

#' @export
setMethod(
  "[",
  signature(x = "Term", i = "numeric"),
  function(x, i, ...) {
    .val <- x@.Data
    term(.val[i], x@units)
  }
)

#' @export
c.Term <- function(x, ...) {
  dots <- list(...)
  nempty <- sapply(dots, length) != 0
  elements <- dots[nempty]
  values_ <- c(x@.Data, unlist(lapply(elements, as.numeric)))
  term(values_, x@units)
}