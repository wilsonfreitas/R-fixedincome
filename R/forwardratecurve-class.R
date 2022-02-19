
#' @export
setClass(
  "ForwardRateCurve",
  slots = c(
    terms = "ANY",
    refdate = "Date"
  ),
  validity = function(object) {
    len_check <- length(object@terms) == length(object@.Data)
    terms_positivity_check <- all(object@terms > 0)
    terms_na_check <- ! any(is.na(object@terms))
    len_check & terms_positivity_check & terms_na_check
  },
  contains = "SpotRate"
)

#' @export
forwardratecurve <- function(x, ...) {
  UseMethod("forwardratecurve")
}

#' @export
forwardratecurve.numeric <- function(x, terms, .compounding, .daycount,
                                     .calendar = "actual", refdate = Sys.Date(),
                                     .copyfrom = NULL) {
  .underlying <- spotrate(.value = x,
                          .compounding = .compounding,
                          .daycount = .daycount,
                          .calendar = .calendar,
                          .copyfrom = .copyfrom)
  
  .Object <- new("ForwardRateCurve",
                 .Data = .underlying@.Data,
                 compounding = .underlying@compounding,
                 daycount = .underlying@daycount,
                 calendar = .underlying@calendar,
                 terms = terms,
                 refdate = refdate)
  
  validObject(.Object)
  .Object
}

#' @export
forwardratecurve.SpotRate <- function(x, terms,
                                      refdate = Sys.Date(),
                                      .copyfrom = NULL) {
  
  .Object <- new("ForwardRateCurve",
                 .Data = x@.Data,
                 compounding = x@compounding,
                 daycount = x@daycount,
                 calendar = x@calendar,
                 terms = terms,
                 refdate = refdate)
  
  validObject(.Object)
  .Object
}

#' @export
forwardratecurve.SpotRateCurve <- function(x, .copyfrom = NULL) {

  if (length(x) == 1) {
    .Object <- new("ForwardRateCurve",
                   .Data = x@.Data,
                   compounding = x@compounding,
                   daycount = x@daycount,
                   calendar = x@calendar,
                   terms = x@terms,
                   refdate = x@refdate)
  } else {
    factor_rate <- compound(x)
    factor_rate_b <- shift(factor_rate)
    dub <- diff(x@terms, fill = NA)
    tf <- timefactor(x@daycount, dub)
    # first element is NA
    rates_ <- rates(x@compounding, tf, factor_rate / factor_rate_b)[-1]
    
    fwd_curve <- forwardratecurve(
      rates_,
      dub[-1],
      x@compounding,
      x@daycount,
      x@calendar,
      refdate = x@refdate
    )
    
    .Object <- c(forwardratecurve(x[1]), fwd_curve)
  }
  
  validObject(.Object)
  .Object
}

#' @export
setMethod(
  "[",
  signature(x = "ForwardRateCurve", i = "numeric", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    forwardratecurve(x@.Data[i], x@terms[i], x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setMethod(
  "[",
  signature(x = "ForwardRateCurve", i = "missing", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    forwardratecurve(x@.Data, x@terms, x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setReplaceMethod(
  "[",
  signature(x="ForwardRateCurve", i="numeric", j="missing", value="SpotRate"),
  function(x, i, j, ..., value) {
    x[i] <- value@.Data
    x
  }
)


#' @export
setMethod(
  "as.data.frame",
  signature(x = "ForwardRateCurve"),
  function(x, row.names = NULL, optional = FALSE, ...) {
    spotrate_ <- spotrate(.value = x@.Data,
                          .compounding = x@compounding,
                          .daycount = x@daycount,
                          .calendar = x@calendar)
    data.frame(terms = x@terms, rates = spotrate_)
  }
)

#' @export
setMethod(
  "show",
  "ForwardRateCurve",
  function(object) {
    hdr <- paste(as(object@compounding, "character"), as(object@daycount, "character"), object@calendar)
    
    m <- as.matrix(object@.Data, ncol=1)
    .terms <- term(object@terms, "days")
    rownames(m) <- as.character(.terms)
    colnames(m) <- "ForwardRateCurve"
    print.default(m)
    cat(hdr)
    cat("\n")
    cat("Reference date:", format(object@refdate), "\n")
    invisible(object)
  }
)

#' @export
setMethod(
  "compound",
  signature(x = "ForwardRateCurve", .t = "missing", .v = "missing"),
  function(x, .t, .v) {
    compound(x, x@terms)
  }
)

#' @export
setMethod(
  "c",
  signature(x = "ForwardRateCurve"),
  function(x, ...) {
    dots <- list(...)
    elements <- lapply(dots, spr_builder(x))
    values_ <- c(x@.Data, unlist(lapply(elements, as.numeric)))
    rates_ <- spotrate(values_, x@compounding, x@daycount, x@calendar)
    terms_ <- c(x@terms, unlist(lapply(dots, function(dx) dx@terms)))
    forwardratecurve(rates_, terms_, refdate = x@refdate)
  }
)

