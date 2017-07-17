
#' @export
term <- setClass(
  "term",
  slots = c(units = "character"),
  prototype = prototype(units = "day"),
  contains = "Period"
)

#' @export term
setMethod(
  "initialize",
  "term",
  function(.Object, value, unit) {
    lx <- length(value)
    
    unit <- sub("^(.*)s$", "\\1", unit)
    stopifnot(all(unit %in% c('year', 'month', 'day')))
    unit <- rep_len(unit, lx)
    
    x <- matrix(0, nrow = lx, ncol = 3)
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
    
    slot(.Object, "hour") <- numeric(lx)
    slot(.Object, "minute") <- numeric(lx)
    slot(.Object, ".Data") <- numeric(lx)
    
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
setMethod("units", "term", function(x) x@units)

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
setMethod("as.numeric", "term", function(x) unname(find_out_term_value(x)))

setAs("term", "numeric", function(from) as.numeric(from))

#' @export
setMethod("as.character", "term", function(x) format(x))

setAs("term", "character", function(from) as.character(from))

#' @export
format.term <- function(x, ...) {
  unit <- units(x)
  value <- find_out_term_value(x)
  paste(value, unit)
}

#' @export
setGeneric("as.term", function(x, ...) standardGeneric("as.term"))

setMethod(
  "as.term", "character",
  function(x, ...) {
    .p <- as.period(x)
    .val <- find_out_term_value(.p)
    .unit <- find_out_period_unit(.p)
    term(.val, .unit)
  }
)

setAs("character", "term", function(from) as.term(from))

setAs("term", "Period", function(from) {
  .ch <- as.character(from)
  as.period(.ch)
})
