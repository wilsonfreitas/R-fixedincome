
#' @export
spotrate <- setClass(
  "spotrate",
  representation = representation(
    value = "numeric",
    compounding = "compounding",
    daycount = "daycount",
    calendar = "character"
  )
)

#' @export spotrate
setMethod(
  "initialize",
  "spotrate",
  function(.Object, .value, .compounding, .daycount, .calendar = "actual", copyfrom = NULL) {
    if ( !is.null(copyfrom) ) {
      .value <- if (missing(.value)) copyfrom@value else .value
      .compounding <- if (missing(.compounding)) copyfrom@compounding else .compounding
      .daycount <- if (missing(.daycount)) copyfrom@daycount else .daycount
      .calendar <- if (missing(.calendar)) copyfrom@calendar else .calendar
    }
    .compounding <- if (is(.compounding, "character")) compounding(.compounding) else .compounding
    .daycount <- if (is(.daycount, "character")) daycount(.daycount) else .daycount
    .Object@value <- as.numeric(.value)
    .Object@compounding <- .compounding
    .Object@daycount <- .daycount
    .Object@calendar <- .calendar
    .Object
  }
)

# coercion ----

#' @export
setMethod("as.numeric", "spotrate", function(x) x@value)

#' @export
setMethod(
  "as.character", "spotrate",
  function(x) {
    paste(x@value, as(x@compounding, "character"), as(x@daycount, "character"), x@calendar)
  }
)

#' @export
setMethod(
  "as.list", "spotrate",
  function(x) {
    list(
      value = x@value,
      compounding = as(x@compounding, "character"),
      daycount = as(x@daycount, "character"),
      calendar = x@calendar
    )
  }
)


setAs("spotrate", "character", function(from) as.character(from))
setAs("spotrate", "numeric", function(from) as.numeric(from))
setAs("spotrate", "list", function(from) as.list(from))

#' @export
setGeneric(
  "as.spotrate",
  function(x, ...) {
    standardGeneric("as.spotrate")
  }
)

.parse_spotrate <- function(x) {
  lapply(strsplit(x, '\\s+', perl=TRUE), function (x) {
    if (length(x) != 4)
      stop("Invalid spotrate specification")
    spotrate(as.numeric(x[1]), x[2], x[3], x[4])
  })
}

#' @export
setMethod(
  "as.spotrate",
  "character",
  function(x, simplify = TRUE) {
    if (simplify) {
      m <- regexec("^(\\d+\\.\\d+)\\s+(.*)$", x)
      rm <- regmatches(x, m)
      specs <- unique(sapply(rm, function(x) x[3]))
      if (length(specs) == 1) {
        value <- sapply(rm, function(x) as.numeric(x[2]))
        specs <- strsplit(specs, '\\s+')[[1]]
        if (length(specs) != 3)
          stop("Invalid spotrate specification")
        spotrate(value, specs[1], specs[2], specs[3])
      } else .parse_spotrate(x)
    } else .parse_spotrate(x)
  }
)

#' @export
setMethod(
  "as.spotrate",
  "list",
  function(x, ...) {
    spotrate(x$value, x$compounding, x$daycount, x$calendar)
  }
)

# print, show and format

#' @export
setMethod(
  "format", signature("spotrate"),
  function(x, ...) {
    hdr <- paste(as(x@compounding, "character"), as(x@daycount, "character"), x@calendar)
    paste(callGeneric(x@value, ...), hdr)
  }
)

#' @export
setMethod(
  "show", signature("spotrate"),
  function(object) {
    hdr <- paste(as(object@compounding, "character"), as(object@daycount, "character"), object@calendar)
    cat(hdr, "\n")
    cat(object@value, "\n")
  }
)

# [spotrate], term:[numeric], units:[character]
# [spotrate], [term]
# [spotrate], from, to

#' @export
setMethod(
  f = "compound",
  signature = c("spotrate", "numeric", "character"),
  def = function(x, .t, .v) {
    tm <- term(.t, .v)
    tf <- timefactor(x@daycount, tm)
    compound(x@compounding, tf, x@value)
  }
)

#' @export
setMethod(
  f = "compound",
  signature = c("spotrate", "term", "missing"),
  def = function(x, .t, .v) {
    tf <- timefactor(x@daycount, .t)
    compound(x@compounding, tf, x@value)
  }
)

#' @export
setMethod(
  f = "compound",
  signature = c("spotrate", "Date", "Date"),
  def = function(x, .t, .v) {
    tm <- term(bizdays::bizdays(.t, .v, x@calendar), "days")
    tf <- timefactor(x@daycount, tm)
    compound(x@compounding, tf, x@value)
  }
)

#' @export
setGeneric(
  name = "discount",
  def = function(x, .t, .v, ...) {
    standardGeneric("discount")
  }
)

#' @export
setMethod(
  f = "discount",
  signature = c("spotrate", "numeric", "character"),
  def = function(x, .t, .v) {
    1 / compound(x, .t, .v)
  }
)

#' @export
setMethod(
  f = "discount",
  signature = c("spotrate", "term", "missing"),
  def = function(x, .t, .v) {
    1 / compound(x, .t, .v)
  }
)

#' @export
setMethod(
  f = "discount",
  signature = c("spotrate", "Date", "Date"),
  def = function(x, .t, .v) {
    1 / compound(x, .t, .v)
  }
)

# Ops: Arith, Compare methods ----

#' @export
setMethod(
  "Arith", signature("spotrate", "spotrate"),
  function(e1, e2) {
    e1@value <- callGeneric(e1@value, e2@value)
    e1
  }
)

#' @export
setMethod(
  "Arith", signature("spotrate", "numeric"),
  function(e1, e2) {
    e1@value <- callGeneric(e1@value, e2)
    e1
  }
)

#' @export
setMethod(
  "Arith", signature("numeric", "spotrate"),
  function(e1, e2) {
    e2@value <- callGeneric(e1, e2@value)
    e2
  }
)

#' @export
setMethod(
  "Compare", signature("spotrate", "spotrate"),
  function(e1, e2) {
    callGeneric(e1@value, e2@value)
  }
)

#' @export
setMethod(
  "Compare", signature("spotrate", "numeric"),
  function(e1, e2) {
    callGeneric(e1@value, e2)
  }
)

#' @export
setMethod(
  "Compare", signature("numeric", "spotrate"),
  function(e1, e2) {
    callGeneric(e1, e2@value)
  }
)

# other methods

#' @export
setMethod(
  "is.na", signature("spotrate"),
  function(x) {
    is.na(x@value)
  }
)

#' @export
setMethod(
  "is.infinite", signature("spotrate"),
  function(x) {
    is.infinite(x@value)
  }
)

#' @export
setMethod(
  "[",
  signature("spotrate"),
  function(x, i) {
    spotrate(x@value[i], x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setReplaceMethod(
  "[",
  signature("spotrate"),
  function(x, i, value) {
    x@value[i] <- value
    x
  }
)

#' @export
setMethod(
  "length",
  "spotrate",
  function(x) {
    length(x@value)
  }
)

#' @export
setMethod(
  "append", c("spotrate", "numeric"),
  function(x, values, after = length(x)) {
    values_ <- append(x@value, values, after)
    spotrate(values_, x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setMethod(
  "append", c("spotrate", "spotrate"),
  function(x, values, after = length(x)) {
    values_ <- append(x@value, values@value, after)
    spotrate(values_, x@compounding, x@daycount, x@calendar)
  }
)

# convert

#' @export
setGeneric("convert", function(x, .t1, .t2, ...) standardGeneric("convert"))

#' @export
setMethod(
  "convert",
  c("spotrate", "term", "missing"),
  function (x, .t1, .t2, .compounding, .daycount, .calendar) {
    .compounding <- if (missing(.compounding)) x@compounding else .compounding
    .compounding <- if (is(.compounding, "character")) compounding(.compounding) else .compounding
    
    .daycount <- if (missing(.daycount)) x@daycount else .daycount
    .daycount <- if (is(.daycount, "character")) daycount(.daycount) else .daycount
    
    .calendar <- if (missing(.calendar)) x@calendar else .calendar
    
    .value <- rates(.compounding, timefactor(.daycount, .t1), compound(x, .t1))
    spotrate(.value, .compounding, .daycount, .calendar)
  }
)

#' @export
setMethod(
  "convert",
  c("spotrate", "Date", "Date"),
  function (x, .t1, .t2, .compounding, .daycount, .calendar) {
    .compounding <- if (missing(.compounding)) x@compounding else .compounding
    .compounding <- if (is(.compounding, "character")) compounding(.compounding) else .compounding
    
    .daycount <- if (missing(.daycount)) x@daycount else .daycount
    .daycount <- if (is(.daycount, "character")) daycount(.daycount) else .daycount
    
    .calendar <- if (missing(.calendar)) x@calendar else .calendar
    
    tm2 <- term(bizdays::bizdays(.t1, .t2, .calendar), "days")
    .value <- rates(.compounding, timefactor(.daycount, tm2), compound(x, .t1, .t2))
    spotrate(.value, .compounding, .daycount, .calendar)
  }
)

#' @export
setMethod(
  "rep",
  "spotrate",
  function(x, times) {
    n <- rep(x@value, times)
    spotrate(n, x@compounding, x@daycount, x@calendar)
  }
)

# setMethod(
#   "head",
#   "spotrate",
#   function(x, n = 6L) {
#     y <- head(x@value, n)
#     spotrate(y, x@compounding, x@daycount, x@calendar)
#   }
# )
