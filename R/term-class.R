
#' Term class
#' 
#' It is the time interval used in calculations with interest rates.
#' 
#' 
#' @export
setClass(
  "Term",
  slots = c(value = "numeric", units = "character"),
  prototype = prototype(units = "day", value = 1)
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
    slot(.Object, "value") <- value
    
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

#' @export
as.data.frame.Term <- function (x, row.names = NULL, optional = FALSE,
                                ..., nm = deparse1(substitute(x)))  {
  force(nm)
  nrows <- length(x)
  if (!(is.null(row.names) || (is.character(row.names) && length(row.names) == nrows))) {
    warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!",
                     nrows), domain = NA)
    row.names <- NULL
  }
  if (is.null(row.names)) {
    if (nrows == 0L)
      row.names <- character()
    else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names))
      row.names <- .set_row_names(nrows)
  }
  value <- list(x)
  if (!optional)
    names(value) <- nm
  structure(value, row.names = row.names, class = "data.frame")
}

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
  unit <- units(x)
  value <- x@value
  abrev <- sapply(unit, .select_unit)
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
term <- function(value, units = "days") {
  new("Term", value = value, units = units)
}
