
#' Term class
#' 
#' It is the time interval used in calculations with interest rates.
#' The term class represents the period used to discount or compound a spot 
#' rate.
#' 
#' @param x can be a numeric value representing the time period or the initial
#' date for a period between two dates.
#' @param units one of the valid \code{units}: \code{days}, \code{monts}, 
#' \code{years}.
#' @param end_date the final date for a period between two dates.
#' @param calendar the calendar used to compute the amount of days for a period
#' between two dates.
#' 
#' @name term-class
#' @examples
#' term(6, "months")
#' term(as.Date("2022-02-02"), as.Date("2022-02-23"), "Brazil/ANBIMA")
#' 
NULL

#' @export
term <- function(x, ...) {
  UseMethod("term")
}

#' @rdname term-class
#' @export
term.numeric <- function(x, units = "days") {
  value <- x
  
  if (length(units) > 1) {
    warning("units length > 1 and only the first element will be used")
    units <- units[1]
  }
  
  units <- sub("^(.*)s$", "\\1", units)
  stopifnot(units %in% c('year', 'month', 'day'))
  
  new("Term", .Data = value, units = units)
}

#' @export
term.Term <- function(x, ...) {
  x
}

#' @rdname term-class
#' @export
term.Date <- function(x, end_date, calendar) {
  start_date <- x
  new("DateRangeTerm", bizdays(start_date, end_date, calendar),
      start_date = start_date, end_date = end_date, calendar = calendar,
      units = "day")
}

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

#' Term coercion
#' 
#' The Term class has a string representation and can be coerced to a character.
#' Also a character can be coerced to a Term object.
#' 
#' @param x a Term object or a character representing a term.
#' 
#' @details
#' The string representation of the Term class follows the layout:
#' \code{<number> <units>}
#' where units is one of: days, months, years.
#' 
#' @name term-coercion
#' @examples
#' t <- as.term("6 months")
#' as.character(t)
#' 
NULL


#' @rdname term-coercion
#' @export
setMethod(
  "as.character",
  signature(x = "Term"),
  function(x) {
    format(x)
  }
)

#' @rdname term-coercion
#' @export
setGeneric(
  "as.term",
  function(x, ...) {
    standardGeneric("as.term")
  }
)

#' @rdname term-coercion
setMethod(
  "as.term",
  signature(x = "character"),
  function(x, ...) {
    m <- regexec('^([0-9]+)(\\.[0-9]+)? (years|months|days|year|month|day)?$', x)
    m <- unlist(regmatches(x, m))
    if (length(m))
      term(as.numeric(paste0(m[2], m[3])), m[4])
    else
      stop("Invalid term: ", x)
  }
)

# methods ----

#' Term methods
#' 
#' The Term class has a few methods that helps in tasks involving data
#' manipulation.
#' 
#' @param x a Term object.
#' @param i index used with the \code{[]} operator. Can be numeric
#' (positional index) or boolean (\code{logical}), similar to vector indexing.
#' @param k a numeric with the number of elements to shift the Term vector
#' @param lag a numerix indicating which lag to use.
#' @param fill a numeric value (or \code{NA}) to fill the empty created by
#' applying shift or diff to a Term vector.
#' 
#' @details
#' Indexing a Term with the \code{[]} operator is similar to vector indexing.
#' The only difference is that the returned object is of Term class.
#' 
#' \code{c} concatenates terms and keeps the units correct.
#' 
#' \code{format} coerces the Term class to a string representation that is
#' compatible with the one used to visualize data.frames.
#' 
#' \code{shift} shifts the Term vector elements by \code{k} positions.
#' 
#' \code{diff} returns a Term vector with lagged differences.
#' 
#' @name term-methods
#' @examples
#' t <- as.term("6 months")
#' as.character(t)
#' 
NULL

#' @rdname term-methods
#' @export
setMethod(
  "[",
  signature(x = "Term"),
  function(x, i, ...) {
    .val <- x@.Data
    term(.val[i], x@units)
  }
)

#' @rdname term-methods
#' @export
setMethod(
  "c",
  signature(x = "Term"),
  function(x, ...) {
    dots <- list(...)
    nempty <- sapply(dots, length) != 0
    elements <- dots[nempty]
    values_ <- c(x@.Data, unlist(lapply(elements, as.numeric)))
    term(values_, x@units)
  }
)

#' @rdname term-methods
#' @export
format.Term <- function(x, ...) {
  value <- x@.Data
  abrev <- x@units
  abrev <- ifelse(value > 1, paste0(abrev, "s"), abrev)
  paste(value, abrev, sep =" ")
}

#' @rdname term-methods
#' @export
setMethod(
  "shift",
  signature(x = "Term"),
  function(x, k = 1, ..., fill = NA) {
    shifted <- shift(as.numeric(x), k, fill = fill)
    term(shifted, x@units)
  }
)

#' @rdname term-methods
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

