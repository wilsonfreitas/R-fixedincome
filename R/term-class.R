
#' Term class
#' 
#' It is the time interval used in calculations with interest rates.
#' 
#' 
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
  "[",
  signature(x = "Term"),
  function(x, i, j, ..., drop = TRUE) {
    .val <- x@.Data
    term(.val[i], x@units)
  }
)

# coercion 1: from Term to ANY ----

#' @export
setMethod(
  "as.character",
  signature(x = "Term"),
  function(x) {
    format(x)
  }
)

# coercion 2: from ANY to Term ----

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
    m <- regexec('^([0-9]+)(\\.[0-9]+)? (years|months|days|year|month|day)?$', x)
    m <- unlist(regmatches(x, m))
    if (length(m))
      term(as.numeric(paste0(m[2], m[3])), m[4])
    else
      stop("Invalid term: ", x)
  }
)

# methods ----

#' @export
length.Term <- function(x) {
  length(x@.Data)
}

#' @export
format.Term <- function(x, ...) {
  value <- x@.Data
  abrev <- x@units
  abrev <- ifelse(value > 1, paste0(abrev, "s"), abrev)
  paste(value, abrev, sep =" ")
}

#' @export
setMethod(
  "show",
  signature("Term"),
  function(object) {
    print(format(object))
  }
)

#' @export
setMethod(
  "shift",
  signature(x = "Term"),
  function(x, k = 1, ..., fill = NA) {
    shifted <- shift(as.numeric(x), k, fill = fill)
    term(shifted, x@units)
  }
)

#' @export
setMethod(
  "diff",
  signature(x = "Term"),
  function(x, ..., fill = NULL) {
    diff_x <- as.numeric(diff(x@.Data))
    if (is.null(fill)) {
      term(diff_x, x@units[-1])
    } else {
      term(c(fill, diff_x), x@units)
    }
  }
)

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

#' @export
term <- function(x, ...) {
  UseMethod("term")
}

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

#' @export
term.Date <- function(x, end_date, calendar) {
  start_date <- x
  new("DateRangeTerm", bizdays(start_date, end_date, calendar),
      start_date = start_date, end_date = end_date, calendar = calendar,
      units = "day")
}

