
#' Term class
#' 
#' It is the time interval used in calculations with interest rates.
#' 
#' 
#' @export
setClass(
  "Term",
  slots = c(units = "character"),
  prototype = prototype(units = "day"),
  contains = "numeric"
)

setMethod(
  "initialize",
  "Term",
  function(.Object, ...) {
    dots <- list(...)
    value <- as.integer(dots[[1]])
    units <- dots[[2]]
    
    max_len <- max(length(value), length(units))
    
    units <- sub("^(.*)s$", "\\1", units)
    stopifnot(all(units %in% c('year', 'month', 'day')))
    units <- rep_len(units, max_len)
    value <- rep_len(value, max_len)

    slot(.Object, "units") <- units
    slot(.Object, ".Data") <- value
    
    validObject(.Object)
    .Object
  }
)

#' @export
units.Term <- function(x) {
  x@units
}

#' @export
setMethod(
  "units",
  signature(x = "Term"),
  units.Term
)

#' @export
setMethod(
  "[",
  signature(x = "Term"),
  function(x, i, j, ..., drop = TRUE) {
    .val <- x@value
    .unit <- units(x)
    term(.val[i], .unit[i])
  }
)

# coercion 1: from Term to ANY ----

#' @export
setMethod(
  "as.numeric",
  signature(x = "Term"),
  function(x) {
    unname(x@value)
  }
)

setAs(
  "Term",
  "numeric",
  function(from) {
    as.numeric(from)
  }
)

#' @export
setMethod(
  "as.character",
  signature(x = "Term"),
  function(x) {
    format(x)
  }
)

setAs(
  "Term",
  "character",
  function(from) {
    as.character(from)
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

as.term.character <- function(x, units=NULL, ...) {
  m <- regexec('^([0-9]+)(\\.[0-9]+)? (years|months|days)?$', x)
  m <- unlist(regmatches(x, m))
  if (length(m))
    t <- as.term(as.numeric(paste0(m[2], m[3])), m[4])
  else
    stop("Invalid term: ", x)
  if (is.null(units)) t
  else as.term(t, units)
}


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

setAs(
  "character",
  "Term",
  function(from) {
    as.term(from)
  }
)

setAs(
  "Term",
  "Period",
  function(from) {
    .ch <- as.character(from)
    lubridate::as.period(.ch)
  }
)

# methods ----

.select_unit <- function(x) {
  switch(x, year = "year", month = "month", day = "day")
}

#' @export
length.Term <- function(x) {
  length(x@value)
}

#' @export
setMethod(
  "length",
  signature("Term"),
  length.Term
)

#' @export
format.Term <- function(x, ...) {
  value <- x@.Data
  abrev <- units(x)
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

# TODO in compare it should take into account the periods: year, day and month
#      year > month > day
#      with a daycount a size for months and year can be assumed
#' @export
setMethod(
  "Compare",
  signature("Term", "Term"),
  function(e1, e2) {
    callGeneric(as.character(e1), as.character(e2))
  }
)

#' @export
setMethod(
  "Compare",
  signature("Term", "character"),
  function(e1, e2) {
    callGeneric(as.character(e1), e2)
  }
)

#' @export
setMethod(
  "Compare",
  signature("character", "Term"),
  function(e1, e2) {
    callGeneric(as.character(e2), e1)
  }
)


#' @export
term <- function(x, units = "days") {
  new("Term", x, units = units)
}
