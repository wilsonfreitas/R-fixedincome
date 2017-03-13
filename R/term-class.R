
#' @exportClass difftime
setOldClass("difftime")

#' @export
term <- setClass(
  "term",
  representation = representation(value = "numeric", units = "character"),
  validity = function(object) {
    valid_units <- object@units %in% c('years', 'months', 'days')
    valid_value <- all(object@value >= 0)
    return(valid_value & valid_units)
  },
  contains = "difftime"
)

#' @export term
setMethod(
  "initialize",
  "term",
  function(.Object, value, units) {
    .Object@value <- value
    .Object@units <- units
    if (validObject(.Object))
      return(.Object)
  }
)


# coercion ----

#' @export
setMethod("as.numeric", "term", function(x) x@value)

setAs("term", "numeric", function(from) as.numeric(from))

#' @export
setMethod("as.character", "term", function(x) {
  paste(x@value, x@units)
})

setAs("term", "character", function(from) as.character(from))

#' @export
setGeneric("as.term", function(x, ...) standardGeneric("as.term"))

setMethod(
  "as.term", "character",
  function(x, simplify = TRUE) {
    m <- regexec('^([0-9]+)(\\.[0-9]+)? (years|months|days)?$', x)
    rm <- regmatches(x, m)
    if (simplify) {
      units <- unique(sapply(rm, function(x) x[4]))
      if (is.na(units))
        stop("Invalid term.")
      if (length(units) == 1) {
        value <- sapply(rm, function(x) as.numeric(paste0(x[2], x[3])))
        term(value, units)
      } else
        lapply(rm, function(x) term(as.numeric(paste0(x[2], x[3])), x[4]))
    } else
      lapply(rm, function(x) term(as.numeric(paste0(x[2], x[3])), x[4]))
  }
)

setAs("character", "term", function(from) as.term(from))

# show

#' @export
setMethod("show", "term", function(object) {
  hdr <- paste('terms in', object@units)
  x <- paste(hdr, paste(object@value, collapse=' '), sep='\n')
  cat(x)
})

# other methods ----

#' @export
setMethod("units", "term", function(x) x@units)

# Compare methods ----

#' @export
setMethod(
  "Compare", signature("term", "term"),
  function(e1, e2) {
    callGeneric(e1@value, e2@value) & callGeneric(e1@units, e2@units)
  }
)

#' @export
setMethod(
  "Compare", signature("term", "numeric"),
  function(e1, e2) {
    callGeneric(e1@value, e2)
  }
)

#' @export
setMethod(
  "Compare", signature("numeric", "term"),
  function(e1, e2) {
    callGeneric(e1, e2@value)
  }
)

#' @export
setMethod(
  "Arith", signature("term", "term"),
  function(e1, e2) {
    if (e1@units == e2@units) {
      x <- callGeneric(e1@value, e2@value)
      term(x, e1@units)
    } else
      stop("Can't execute arithmetic operations in terms with different units")
  }
)

#' @export
setMethod(
  "Arith", signature("term", "numeric"),
  function(e1, e2) {
    x <- callGeneric(e1@value, e2)
    term(x, e1@units)
  }
)

#' @export
setMethod(
  "Arith", signature("numeric", "term"),
  function(e1, e2) {
    x <- callGeneric(e1, e2@value)
    term(x, e2@units)
  }
)

# vector methods ----

#' @export
setMethod("length", "term", function(x) length(x@value))
