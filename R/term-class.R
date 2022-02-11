
#' @export
term <- setClass(
  "term",
  slots = c(units = "character", day = "integer", month = "integer", year = "integer"),
  prototype = prototype(units = "day", day = 0L, month = 0L, year = 0L),
  contains = "matrix"
)

#' @export term
setMethod(
  "initialize",
  "term",
  function(.Object, ...) {
    dots <- list(...)
    value <- as.integer(dots[[1]])
    unit <- dots[[2]]
    
    lx <- max(length(value), length(unit))
    
    unit <- sub("^(.*)s$", "\\1", unit)
    stopifnot(all(unit %in% c('year', 'month', 'day')))
    unit <- rep_len(unit, lx)
    
    value <- rep_len(value, lx)
    
    x <- matrix(0L, nrow = lx, ncol = 3)
    colnames(x) <- c("day", "month", "year")
    
    ix <- unit == "day"
    x[ix, "day"] <- value[ix]
    ix <- unit == "month"
    x[ix, "month"] <- value[ix]
    ix <- unit == "year"
    x[ix, "year"] <- value[ix]
    
    slot(.Object, "units") <- unit
    
    slot(.Object, "day") <-   unname(x[, "day"])
    slot(.Object, "month") <- unname(x[, "month"])
    slot(.Object, "year") <-  unname(x[, "year"])
    
    # slot(.Object, "hour") <- numeric(lx)
    # slot(.Object, "minute") <- numeric(lx)
    slot(.Object, ".Data") <- x
    
    validObject(.Object)
    .Object
  }
)

find_out_term_value <- function(x) {
  x@day + x@month + x@year
}

find_out_period_unit <- function(x) {
  ix_day <- x@day != 0
  ix_month <- x@month != 0
  ix_year <- x@year != 0
  unit <- character(length(x@.Data))
  unit[ix_day] <- "day"
  unit[ix_month] <- "month"
  unit[ix_year] <- "year"
  unit
}

#' @export
setMethod("units", signature(x = "term"), function(x) x@units)

#' #' @export
#' setMethod("c", signature(x = "term"), function(x, ...){
#'   elements <- lapply(list(...), as.period)
#'   seconds <- c(x@.Data, unlist(lapply(elements, slot, ".Data")))
#'   years <- c(x@year, unlist(lapply(elements, slot, "year")))
#'   months <- c(x@month, unlist(lapply(elements, slot, "month")))
#'   days <- c(x@day, unlist(lapply(elements, slot, "day")))
#'   hours <- c(x@hour, unlist(lapply(elements, slot, "hour")))
#'   minutes <- c(x@minute, unlist(lapply(elements, slot, "minute")))
#'   new("term", seconds, year = years, month = months, day = days,
#'       hour = hours, minute = minutes)
#' })
#' 
#' #' @export
#' setMethod("rep", signature(x = "term"), function(x, ...){
#'   new("term", rep(x@.Data, ...), year = rep(x@year, ...),
#'       month = rep(x@month, ...), day = rep(x@day, ...),
#'       hour = rep(x@hour, ...), minute = rep(x@minute, ...))
#' })

#' @export
setMethod("[", signature(x = "term"),
          function(x, i, j, ..., drop = TRUE) {
            .val <- find_out_term_value(x)
            .unit <- units(x)
            term(.val[i], .unit[i])
          })

#' @export
setMethod("[[", signature(x = "term"),
          function(x, i, j, ..., exact = TRUE) {
            .val <- find_out_term_value(x)
            .unit <- units(x)
            term(.val[i], .unit[i])
          })

# coercion ----

#' @export
setMethod("as.numeric", signature(x = "term"), function(x) unname(find_out_term_value(x)))

setAs("term", "numeric", function(from) as.numeric(from))

#' @export
setMethod("as.character", signature(x = "term"), function(x) format(x))

setAs("term", "character", function(from) as.character(from))

#' @export
format.term <- function(x, ...) {
  unit <- units(x)
  value <- find_out_term_value(x)
  abrev <- sapply(unit, function(x) switch(x, year = "year", month = "month", day = "day"))
  paste(value, abrev, sep =" ")
}

#' @export
setGeneric("as.term", function(x, ...) standardGeneric("as.term"))

setMethod(
  "as.term", signature(x = "character"),
  function(x, ...) {
    .p <- lubridate::as.period(x)
    .val <- find_out_term_value(.p)
    .unit <- find_out_period_unit(.p)
    term(.val, .unit)
  }
)

setAs("character", "term", function(from) as.term(from))

setAs("term", "Period", function(from) {
  .ch <- as.character(from)
  lubridate::as.period(.ch)
})

#' @export
setMethod(
  "show",
  signature(object = "term"),
  function(object) {
    print(format(object))
  }
)

#' @export
setMethod(
  "Compare", signature("term", "term"),
  function(e1, e2) {
    all(callGeneric(e1@.Data, e2@.Data))
  }
)

#' @export
setMethod(
  "Compare", signature("term", "character"),
  function(e1, e2) {
    callGeneric(as.character(e1), e2)
  }
)

#' @export
setMethod(
  "Compare", signature("character", "term"),
  function(e1, e2) {
    callGeneric(as.character(e2), e1)
  }
)
