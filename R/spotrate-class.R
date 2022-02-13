
#' @export
setClass(
  "spotrate",
  representation = representation(
    compounding = "Compounding",
    daycount = "Daycount",
    calendar = "character"
  ),
  contains = "numeric"
)

#' @export
spotrate <- function(.value, .compounding, .daycount, .calendar = "actual", .copyfrom = NULL) {
  if ( !is.null(.copyfrom) ) {
    .value <- if (missing(.value)) .copyfrom@.Data else .value
    .compounding <- if (missing(.compounding)) .copyfrom@compounding else .compounding
    .daycount <- if (missing(.daycount)) .copyfrom@daycount else .daycount
    .calendar <- if (.calendar == "actual") .copyfrom@calendar else .calendar
  }
  
  .compounding <- if (is.character(.compounding)) compounding(.compounding) else .compounding
  .daycount <- if (is.character(.daycount)) daycount(.daycount) else .daycount
  new("spotrate", .value, compounding = .compounding, daycount = .daycount, calendar = .calendar)
}

# coercion ----

#' @export
setMethod("as.numeric", "spotrate", function(x) x@.Data)

#' @export
setMethod(
  "as.character", "spotrate",
  function(x) {
    paste(x@.Data, as(x@compounding, "character"), as(x@daycount, "character"), x@calendar)
  }
)

#' @export
setMethod(
  "as.list", "spotrate",
  function(x) {
    list(
      value = x@.Data,
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

# print, show and format ----

# #' @export
# setMethod(
#   "format", signature("spotrate"),
#   function(x, ...) {
#     hdr <- paste(as(x@compounding, "character"), as(x@daycount, "character"), x@calendar)
#     paste(callGeneric(x@.Data, ...), hdr)
#   }
# )

#' @export
setMethod(
  "show", signature("spotrate"),
  function(object) {
    print(format(object))
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
    compound(x@compounding, tf, x@.Data)
  }
)

#' @export
setMethod(
  f = "compound",
  signature = c("spotrate", "Term", "missing"),
  def = function(x, .t, .v) {
    tf <- timefactor(x@daycount, .t)
    compound(x@compounding, tf, x@.Data)
  }
)

#' @export
setMethod(
  f = "compound",
  signature = c("spotrate", "Date", "Date"),
  def = function(x, .t, .v) {
    tm <- term(bizdays::bizdays(.t, .v, x@calendar), "days")
    tf <- timefactor(x@daycount, tm)
    compound(x@compounding, tf, x@.Data)
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
  signature = c("spotrate", "Term", "missing"),
  def = function(x, .t, .v) {
    1 / compound(x, .t)
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
    e1@.Data <- callGeneric(e1@.Data, e2@.Data)
    e1
  }
)

#' @export
setMethod(
  "Arith", signature("spotrate", "numeric"),
  function(e1, e2) {
    e1@.Data <- callGeneric(e1@.Data, e2)
    e1
  }
)

#' @export
setMethod(
  "Arith", signature("numeric", "spotrate"),
  function(e1, e2) {
    e2@.Data <- callGeneric(e1, e2@.Data)
    e2
  }
)

#' @export
setMethod(
  "Compare", signature("spotrate", "spotrate"),
  function(e1, e2) {
    callGeneric(e1@.Data, e2@.Data)
  }
)

#' @export
setMethod(
  "Compare", signature("spotrate", "numeric"),
  function(e1, e2) {
    callGeneric(e1@.Data, e2)
  }
)

#' @export
setMethod(
  "Compare", signature("numeric", "spotrate"),
  function(e1, e2) {
    callGeneric(e1, e2@.Data)
  }
)

# other methods

#' @export
setMethod(
  "is.na", signature("spotrate"),
  function(x) {
    is.na(x@.Data)
  }
)

#' @export
setMethod(
  "is.infinite", signature("spotrate"),
  function(x) {
    is.infinite(x@.Data)
  }
)

#' @export
setMethod(
  "[",
  signature("spotrate"),
  function(x, i) {
    spotrate(x@.Data[i], x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setReplaceMethod(
  "[",
  signature("spotrate"),
  function(x, i, value) {
    x@.Data[i] <- value
    x
  }
)

#' @export
setMethod(
  "length",
  "spotrate",
  function(x) {
    length(x@.Data)
  }
)

# #' @export
# setMethod(
#   "append", c("spotrate", "numeric"),
#   function(x, values, after = length(x)) {
#     values_ <- append(x@.Data, values, after)
#     spotrate(values_, x@compounding, x@daycount, x@calendar)
#   }
# )
# 
# #' @export
# setMethod(
#   "append", c("spotrate", "spotrate"),
#   function(x, values, after = length(x)) {
#     values_ <- append(x@.Data, values@.Data, after)
#     spotrate(values_, x@compounding, x@daycount, x@calendar)
#   }
# )

# convert

#' @export
setGeneric("convert", function(x, .t1, .t2, ...) standardGeneric("convert"))

#' @export
setMethod(
  "convert",
  c("spotrate", "Term", "missing"),
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
    n <- rep(x@.Data, times)
    spotrate(n, x@compounding, x@daycount, x@calendar)
  }
)

# setMethod(
#   "head",
#   "spotrate",
#   function(x, n = 6L) {
#     y <- head(x@.Data, n)
#     spotrate(y, x@compounding, x@daycount, x@calendar)
#   }
# )
