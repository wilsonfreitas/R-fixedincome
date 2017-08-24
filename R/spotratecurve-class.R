
#' @export
setClass(
  "spotratecurve",
  representation = representation(
    terms = "ANY",
    refdate = "Date"
  ),
  validity = function(object) {
    len_check <- length(object@terms) == length(object@.Data)
    unique_check <- length(object@terms) == length(unique(object@terms))
    terms_positivity_check <- all(object@terms > 0)
    len_check & unique_check & terms_positivity_check
  },
  contains = "spotrate"
)

#' @export
spotratecurve <- function(.value, .terms, .compounding, .daycount, .calendar = "actual", .refdate = Sys.Date(), .copyfrom = NULL) {
  if (length(.value) != length(.terms))
    stop("length(.value) must match length(.terms)")
  .underlying <- 
    if (is(.value, "spotrate")) {
      spotrate(.value = .value@.Data,
               .compounding = .value@compounding,
               .daycount = .value@daycount,
               .calendar = .value@calendar,
               .copyfrom = .copyfrom)
    } else {
      spotrate(.value = .value,
               .compounding = .compounding,
               .daycount = .daycount,
               .calendar = .calendar,
               .copyfrom = .copyfrom)
    }
  .Object <- new("spotratecurve",
                 .Data = .underlying@.Data,
                 compounding = .underlying@compounding,
                 daycount = .underlying@daycount,
                 calendar = .underlying@calendar,
                 terms = .terms,
                 refdate = .refdate)
  
  ix <- order(.terms)
  .Object@terms <- .terms[ix]
  .Object@.Data <- .Object@.Data[ix]
  validObject(.Object)
  .Object
}

#' @export
setMethod(
  "[",
  signature(x = "spotratecurve", i = "numeric"),
  function(x, i, strict = FALSE) {
    if (strict) {
      mx <- i
      ix <- x@terms[mx]
    } else {
      if (any(i < 0)) {
        mx <- - match(abs(i), x@terms)
        ix <- x@terms[mx]
      } else {
        mx <- match(i, x@terms)
        ix <- i
      }
    }
    spotratecurve(x@.Data[mx], ix, x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setMethod(
  "[",
  signature(x = "spotratecurve", i = "logical"),
  function(x, i) {
    spotratecurve(x@.Data[i], x@terms[i], x@compounding, x@daycount, x@calendar)
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
      x@.Data[contained.to] <- if (length(value) == 1) value else value[contained.from]
    }
    if (any(! contained.from)) {
      value_ <- c(x@.Data, if (length(value) == 1) rep(value, sum(! contained.from)) else value[! contained.from])
      terms_ <- c(x@terms, i[! contained.from])
      ix <- order(terms_)
      x@.Data <- value_[ix]
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
    x[i] <- value@.Data
    x
  }
)

#' @export
setMethod(
  "head",
  "spotratecurve",
  function(x, n = 6L, ...) {
    y <- head(x@.Data, n)
    t <- head(x@terms, n)
    spotratecurve(y, t, x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setMethod(
  "tail",
  "spotratecurve",
  function(x, n = 6L, ...) {
    y <- tail(x@.Data, n)
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
    
    m <- as.matrix(object@.Data, ncol=1)
    .terms <- term(object@terms, "days")
    rownames(m) <- as.character(.terms)
    colnames(m) <- "spotratecurve"
    print.default(m)
    cat(hdr)
    cat("\n")
    cat("Reference date:", format(object@refdate), "\n")
    invisible(object)
  }
)

# coercion ----

#' @export
setMethod(
  "as.data.frame", "spotratecurve",
  function(x, ...) {
    data.frame(terms = x@terms, rates = x@.Data)
  }
)

