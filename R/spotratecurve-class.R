
#' @export
spotratecurve <- setClass(
  "spotratecurve",
  representation = representation(
    terms = "ANY",
    refdate = "Date"
  ),
  validity = function(object) {
    refdate_check <- if (is(object@terms, "Date")) object@refdate != as.Date(0, "1970-01-01") else TRUE
    len_check <- length(object@terms) == length(object@value)
    unique_check <- length(object@terms) == length(unique(object@terms))
    terms_positivity_check <- all(object@terms > 0)
    len_check & refdate_check & unique_check & terms_positivity_check
  },
  contains = "spotrate"
)

#' @export spotratecurve
setMethod(
  "initialize",
  "spotratecurve",
  function(.Object, .value, .terms, .compounding, .daycount, .calendar = "actual", .refdate = NULL, copyfrom = NULL) {
    if (length(.value) != length(.terms))
      stop("length(.value) must match length(.terms)")
    .Object <- if (is(.value, "spotrate")) {
      callNextMethod(.Object = .Object,
                     .value = .value@value,
                     .compounding = .value@compounding,
                     .daycount = .value@daycount,
                     .calendar = .value@calendar,
                     copyfrom = copyfrom)
    } else {
      callNextMethod(.Object = .Object,
                     .value = .value,
                     .compounding = .compounding,
                     .daycount = .daycount,
                     .calendar = .calendar,
                     copyfrom = copyfrom)
    }
    .Object@refdate <- if (is.null(.refdate)) as.Date(0, "1970-01-01") else .refdate
    ix <- order(.terms)
    .Object@terms <- .terms[ix]
    .Object@value <- .Object@value[ix]
    if (validObject(.Object))
      return(.Object)
  }
)

#' @export
setMethod(
  "[",
  signature(x = "spotratecurve", i = "numeric"),
  function(x, i) {
    spotratecurve(x@value[match(i, x@terms)], i, x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setMethod(
  "[",
  signature(x = "spotratecurve", i = "logical"),
  function(x, i) {
    spotratecurve(x@value[i], x@terms[i], x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setReplaceMethod(
  "[",
  signature(x="spotratecurve", i="numeric", j="missing", value="numeric"),
  function(x, i, value) {
    contained.from <- i %in% x@terms
    contained.to <- x@terms %in% i
    if (any(contained.from)) {
      x@value[contained.to] <- if (length(value) == 1) value else value[contained.from]
    }
    if (any(! contained.from)) {
      value_ <- c(x@value, if (length(value) == 1) rep(value, sum(! contained.from)) else value[! contained.from])
      terms_ <- c(x@terms, i[! contained.from])
      ix <- order(terms_)
      x@value <- value_[ix]
      x@terms <- terms_[ix]
    }
    x
  }
)

#' @export
setReplaceMethod(
  "[",
  signature(x="spotratecurve", i="numeric", j="missing", value="spotrate"),
  function(x, i, value) {
    x[i] <- value@value
    x
  }
)

#' @export
setMethod(
  "head",
  "spotratecurve",
  function(x, n = 6L, ...) {
    y <- head(x@value, n)
    t <- head(x@terms, n)
    spotratecurve(y, t, x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setMethod(
  "tail",
  "spotratecurve",
  function(x, n = 6L, ...) {
    y <- tail(x@value, n)
    t <- tail(x@terms, n)
    spotratecurve(y, t, x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setMethod(
  "show",
  "spotratecurve",
  function(object) {
    hdr <- paste(as(object@compounding, "character"), as(object@daycount, "character"), object@calendar)
    
    m <- as.matrix(object@value, ncol=1)
    .terms <- term(object@terms, "days")
    rownames(m) <- as.character(.terms)
    colnames(m) <- NULL
    print.default(m)
    cat(hdr)
    cat("\n")
    invisible(object)
  }
)

# coercion ----

#' @export
setMethod(
  "as.data.frame", "spotratecurve",
  function(x, ...) {
    data.frame(terms = x@terms, rates = x@value)
  }
)

# setMethod("as.numeric", "spotrate", function(x) x@value)
# setMethod(
#   "as.character", "spotrate",
#   function(x) {
#     paste(x@value, as(x@compounding, "character"), as(x@daycount, "character"), x@calendar)
#   }
# )
