
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
#' @return A `Term` object.
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

  if (length(units) != length(x) && length(units) > 1) {
    stop("units and data are different sizes")
  }

  units <- sub("^(.*)s$", "\\1", units)
  stopifnot(unique(units) %in% c("year", "month", "day"))

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
#' The Term class represents the period used to discount or compound a spot
#' rate.
#'
#' The Term object is defined by its numeric value and its unit, that can be
#' `"days"`, `"months"` or `"years"`.
#' For example:
#'
#' ```{r}
#' term(6, "months")
#' ```
#'
#' It represents a period of 6 months.
#' The Term object can also be created from a string representation of a Term.
#'
#' ```{r}
#' as.term("6 months")
#' ```
#'
#' Since the Term object inherits from a `numeric`, it inherits all numeric
#' operations.
#' Numeric values can be summed or subtracted from a Term object numeric part.
#'
#' ```{r}
#' term(1, "days") + 1
#' ```
#'
#' Arithmetic and comparison operations between Term objects work when both
#' have the same units. `+`, `-` and `%%` return a Term, `/` and `%/%` return
#' a numeric and comparisons return a logical.
#'
#' ```{r}
#' term(1, "days") + term(2, "days")
#' term(10, "days") / term(5, "days")
#' term(10, "days") > term(5, "days")
#' ```
#'
#' Terms with different units raise an error, because converting them
#' requires a daycount; use `todays()`, `tomonths()` or `toyears()` first,
#' or evaluate the operation with [with_daycount()].
#'
#' ```{r}
#' try(term(1, "months") + term(12, "days"))
#' todays(daycount("business/252"), term(1, "months")) + term(12, "days")
#' with_daycount("business/252", term(1, "months") + term(12, "days"))
#' ```
#'
#' ## DateRangeTerm objects
#'
#' The DateRangeTerm class inherits Term and defines start and end dates
#' and a calendar to count the amount of working days between these two dates.
#' This is a Term between two dates.
#'
#' ```{r}
#' term(Sys.Date() - 5, Sys.Date(), "Brazil/ANBIMA")
#' ```
#'
#' In financial markets it is fairly usual to evaluate interest rates between
#' two dates.
#'
#' @aliases DateRangeTerm-class Term-class
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
#' @return A `Term` object created from a string.
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
    m <- do.call(rbind, regmatches(x, m))
    if (length(m)) {
      term(as.numeric(paste0(m[, 2], m[, 3])), m[, 4])
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
#' @return
#' A new `Term` object with lagged differences of the given `Term` object.
#'
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
  abrev <- ifelse(is.na(value) | abs(value) > 1, paste0(abrev, "s"), abrev)
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
setMethod(
  "[",
  signature(x = "Term", i = "logical"),
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
  # each element keeps its own units; plain numbers take the units of x
  units_ <- unlist(lapply(c(list(x), elements), function(e) {
    rep_len(if (is(e, "Term")) e@units else x@units[1], length(e))
  }))
  term(values_, if (length(unique(units_)) == 1) units_[1] else units_)
}

# Daycount set by with_daycount(), used to convert Terms with different units.
.term_context <- new.env(parent = emptyenv())

#' Evaluate Term operations with a daycount
#'
#' Operations between Terms with different units need a daycount to convert
#' them: one year has 252 days with `business/252` and 360 days with
#' `actual/360`.
#' `with_daycount()` evaluates `expr` with the given daycount, so
#' arithmetic and comparison between Terms with different units work.
#' The Terms are converted to the smallest unit among them (years to months,
#' months to days).
#' Outside `with_daycount()` these operations raise an error.
#'
#' @param x a `Daycount` object or a string, like `"business/252"`.
#' @param expr the expression to evaluate.
#'
#' @return the value of `expr`.
#'
#' @examples
#' with_daycount("business/252", term(1, "month") + term(12, "days"))
#' with_daycount("business/252", term(252, "days") == term(1, "year"))
#' with_daycount("actual/360", term(252, "days") == term(1, "year"))
#'
#' @export
with_daycount <- function(x, expr) {
  if (!is(x, "Daycount")) x <- daycount(x)
  old <- .term_context$daycount
  .term_context$daycount <- x
  on.exit(.term_context$daycount <- old)
  expr
}

# Puts two Terms in the same units. With different units it needs the
# daycount set by with_daycount(), otherwise it raises an error.
term_align <- function(e1, e2) {
  n <- max(length(e1), length(e2))
  u1 <- rep_len(e1@units, n)
  u2 <- rep_len(e2@units, n)
  if (all(u1 == u2)) {
    units <- if (length(unique(u1)) == 1) u1[1] else u1
    return(list(as.numeric(e1), as.numeric(e2), units))
  }
  dc <- .term_context$daycount
  if (is.null(dc)) {
    stop(
      "Terms with different units: convert them first ",
      "with todays(), tomonths() or toyears(), or use with_daycount()",
      call. = FALSE
    )
  }
  units <- intersect(c("day", "month", "year"), c(u1, u2))[1]
  conv <- function(e) as.numeric(to_unit_(as.numeric(e), e@units, units, dib(dc)))
  list(conv(e1), conv(e2), units)
}

#' @export
setMethod(
  "Arith",
  signature(e1 = "Term", e2 = "Term"),
  function(e1, e2) {
    a <- term_align(e1, e2)
    units <- a[[3]]
    value <- callGeneric(a[[1]], a[[2]])
    switch(.Generic,
      "+" = ,
      "-" = ,
      "%%" = term(value, units),
      "/" = ,
      "%/%" = value,
      stop("Operation ", .Generic, " is not defined for two Terms")
    )
  }
)

#' @export
setMethod(
  "Compare",
  signature(e1 = "Term", e2 = "Term"),
  function(e1, e2) {
    a <- term_align(e1, e2)
    callGeneric(a[[1]], a[[2]])
  }
)

#' @export
setMethod(
  "Ops",
  signature(e1 = "Term", e2 = "Term"),
  function(e1, e2) {
    stop("Operation ", .Generic, " is not defined for two Terms")
  }
)
