
#' @export
setClass(
  "SpotRateCurve",
  slots = c(
    terms = "ANY",
    refdate = "Date"
  ),
  validity = function(object) {
    len_check <- length(object@terms) == length(object@.Data)
    unique_check <- length(object@terms) == length(unique(object@terms))
    terms_positivity_check <- all(object@terms > 0)
    len_check & unique_check & terms_positivity_check
  },
  contains = "SpotRate"
)

#' @export
spotratecurve <- function(.value, .terms, .compounding, .daycount,
                          .calendar = "actual", .refdate = Sys.Date(),
                          .copyfrom = NULL) {
  if (length(.value) != length(.terms))
    stop("length(.value) must match length(.terms)")
  .underlying <- 
    if (is(.value, "SpotRate")) {
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
  .Object <- new("SpotRateCurve",
                 .Data = .underlying,
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

# TODO situation: given index return all NA
#      choices: define an empty spotrate object or raise an error
#' @export
setMethod(
  "[",
  signature(x = "SpotRateCurve", i = "numeric", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    spotratecurve(x@.Data[i], x@terms[i], x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setMethod(
  "[",
  signature(x = "SpotRateCurve", i = "missing", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    x
  }
)

#' @export
setMethod(
  "[[",
  signature(x = "SpotRateCurve", i = "numeric", j = "missing"),
  function(x, i, j, ...) {
    if (any(i < 0)) {
      mx <- - match(abs(i), x@terms)
      ix <- x@terms[mx]
    } else {
      mx <- match(i, x@terms)
      ix <- i
    }
    spotratecurve(x@.Data[mx], ix, x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setMethod(
  "[",
  signature(x = "SpotRateCurve", i = "logical", j="missing"),
  function(x, i, j, ..., drop = TRUE) {
    spotratecurve(x@.Data[i], x@terms[i], x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setReplaceMethod(
  "[",
  signature(x="SpotRateCurve", i="numeric", j="missing", value="numeric"),
  function(x, i, j, ..., value) {
    if (any(i > length(x@.Data)) || any(i < 1))
      stop("Index out of limits")
    x@.Data[i] <- value
    x
  }
)

#' @export
setReplaceMethod(
  "[[",
  signature(x="SpotRateCurve", i="numeric", j="missing", value="numeric"),
  function(x, i, j, ..., value) {
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
  "[[",
  signature(x="SpotRateCurve", i="numeric", j="missing", value="SpotRate"),
  function(x, i, j, ..., value) {
    contained_from <- i %in% x@terms
    contained_to <- x@terms %in% i
    if (any(contained_from)) {
      x@.Data[contained_to] <- if (length(value) == 1) value else value[contained_from]
    }
    if (any(! contained_from)) {
      value_ <- c(x@.Data, if (length(value) == 1) rep(value, sum(! contained_from)) else value[! contained_from])
      terms_ <- c(x@terms, i[! contained_from])
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
  signature(x="SpotRateCurve", i="numeric", j="missing", value="SpotRate"),
  function(x, i, j, ..., value) {
    x[i] <- value@.Data
    x
  }
)

#' @export
setMethod(
  "show",
  "SpotRateCurve",
  function(object) {
    hdr <- paste(as(object@compounding, "character"), as(object@daycount, "character"), object@calendar)
    
    m <- as.matrix(object@.Data, ncol=1)
    .terms <- term(object@terms, "days")
    rownames(m) <- as.character(.terms)
    colnames(m) <- "SpotRateCurve"
    print.default(m)
    cat(hdr)
    cat("\n")
    cat("Reference date:", format(object@refdate), "\n")
    invisible(object)
  }
)
