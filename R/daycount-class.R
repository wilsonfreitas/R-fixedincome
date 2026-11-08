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
#' `business/252`, `30E/360`, ...
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
#' \code{actual/360}, \code{business/252} and \code{30E/360}.
#'
#' With `actual/...` and `business/...` the days between two dates are
#' counted with the calendar of the rate.
#' With `30E/360` (also called Eurobond basis) every month has 30 days:
#' a date on the 31st is moved to the 30th, and the calendar is not used.
#' Dates computed from the terms of a curve, as in `maturities()`, still use
#' the calendar.
#'
#' @param x a character representing a daycount rule, like: \code{business/252},
#'        \code{actual/365}, \code{actual/360}, ...
#' @param ... additional arguments. Currently unused.
#'
#' @return A `Daycount` object.
#'
#' @examples
#' dc <- daycount("actual/360")
#'
#' sr <- spotrate(0.05, "simple", "30E/360", "actual")
#' compound(sr, as.Date("2024-01-31"), as.Date("2024-03-31"))
#' @export
daycount <- function(x, ...) {
  spec_parts <- strsplit(x, "/")[[1]]
  if (length(spec_parts) != 2) {
    stop("Invalid daycount specitication provided (", x, ")")
  }
  # actual/business days are counted by the calendar, 30E/360 by count_days()
  if (!spec_parts[1] %in% c("actual", "business") && x != "30E/360") {
    stop(
      "Unsupported daycount (", x, "): only actual/..., business/... ",
      "and 30E/360 are implemented"
    )
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
#' @return A numeric with daycount's days in base, the number of days in a year
#' used in the convention.
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

#' Convert Term in different units
#'
#' @description
#' `toyears`, `tomonths` and `todays` functions convert Term objects according
#' to Daycount.
#'
#' @details
#' `toyears` returns the given Term in years units.
#' `tomonths` returns the given Term in months units.
#' `todays` returns the given Term in days units.
#'
#' @param x a Daycount object.
#' @param t a Term object.
#'
#' @aliases
#' toyears,Daycount,Term-method
#' tomonths,Daycount,Term-method
#' todays,Daycount,Term-method
#'
#' @return A Term object converted to the units accordingly the used function.
#'
#' @name term-conversion
#' @examples
#' dc <- daycount("actual/360")
#' t <- term(10, "months")
#' toyears(dc, t)
#' tomonths(dc, t)
#' todays(dc, t)
NULL

#' @rdname term-conversion
#' @export
setGeneric(
  "toyears",
  function(x, t) {
    standardGeneric("toyears")
  }
)

#' @rdname term-conversion
#' @export
setGeneric(
  "tomonths",
  function(x, t) {
    standardGeneric("tomonths")
  }
)

#' @rdname term-conversion
#' @export
setGeneric(
  "todays",
  function(x, t) {
    standardGeneric("todays")
  }
)

create_conv_map <- function(dib) {
  dim <- dib / 12
  matrix(
    c(
      1,       dim,    dib,
      1 / dim, 1,      12,
      1 / dib, 1 / 12, 1
    ),
    nrow = 3,
    dimnames = list(
      from = c("day", "month", "year"),
      to = c("day", "month", "year")
    )
  )
}

to_unit_ <- function(term_value, from_units, to_units, dib) {
  ax <- data.frame(term_value, from_units, to_units, stringsAsFactors = FALSE)
  ym_conv_map <- create_conv_map(dib)
  rx <- sapply(split(ax, seq_len(nrow(ax))), function(x) {
    term_value <- x[, 1]
    from_units <- x[, 2]
    to_units <- x[, 3]
    term_value * ym_conv_map[from_units, to_units]
  })
  unname(rx)
}

setMethod(
  "toyears",
  signature(x = "Daycount", t = "Term"),
  function(x, t) {
    t <- to_unit_(as(t, "numeric"), t@units, "year", dib(x))
    term(t, "year")
  }
)

setMethod(
  "tomonths",
  signature(x = "Daycount", t = "Term"),
  function(x, t) {
    t <- to_unit_(as(t, "numeric"), t@units, "month", dib(x))
    term(t, "month")
  }
)

setMethod(
  "todays",
  signature(x = "Daycount", t = "Term"),
  function(x, t) {
    t <- to_unit_(as(t, "numeric"), t@units, "day", dib(x))
    term(t, "day")
  }
)
# Days between two dates for a daycount: 30E/360 counts every month as 30
# days (31st moved to the 30th); the others count with the calendar.
count_days <- function(daycount, start, end, calendar) {
  if (daycount@.Data != "30E/360") {
    return(bizdays(start, end, calendar))
  }
  d1 <- as.POSIXlt(start)
  d2 <- as.POSIXlt(end)
  360 * (d2$year - d1$year) + 30 * (d2$mon - d1$mon) +
    (pmin(d2$mday, 30) - pmin(d1$mday, 30))
}
