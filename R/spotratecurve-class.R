
#' @export
setClass(
  "SpotRateCurve",
  slots = c(
    terms = "ANY",
    refdate = "Date",
    interpolation = "ANY"
  ),
  validity = function(object) {
    len_check <- length(object@terms) == length(object@.Data)
    unique_check <- length(object@terms) == length(unique(object@terms))
    terms_positivity_check <- all(object@terms > 0)
    terms_na_check <- ! any(is.na(object@terms))
    len_check & unique_check & terms_positivity_check & terms_na_check
  },
  contains = "SpotRate"
)

#' @export
spotratecurve <- function(x, terms, ..., refdate = Sys.Date()) {
  UseMethod("spotratecurve")
}

#' @export
spotratecurve.numeric <- function(x, terms, compounding, daycount,
                                  calendar = "actual", refdate = Sys.Date(),
                                  .copyfrom = NULL) {
  .underlying <- spotrate(.value = x,
                          compounding = compounding,
                          daycount = daycount,
                          calendar = calendar,
                          .copyfrom = .copyfrom)
  
  .Object <- new("SpotRateCurve",
                 .Data = .underlying@.Data,
                 compounding = .underlying@compounding,
                 daycount = .underlying@daycount,
                 calendar = .underlying@calendar,
                 terms = terms,
                 refdate = refdate)
  
  ix <- order(terms)
  .Object@terms <- term(terms[ix], "days")
  .Object@.Data <- .Object@.Data[ix]
  validObject(.Object)
  .Object
}

#' @export
spotratecurve.SpotRate <- function(x, terms,
                                   refdate = Sys.Date(),
                                   .copyfrom = NULL) {
  
  .Object <- new("SpotRateCurve",
                 .Data = x@.Data,
                 compounding = x@compounding,
                 daycount = x@daycount,
                 calendar = x@calendar,
                 terms = terms,
                 refdate = refdate)
  
  ix <- order(terms)
  .Object@terms <- term(terms[ix], "days")
  .Object@.Data <- .Object@.Data[ix]
  validObject(.Object)
  .Object
}

#' @export
as.spotratecurve <- function(x, ...) {
  UseMethod("as.spotratecurve")
}

#' @export
as.spotratecurve.ForwardRate <- function(x, refdate = Sys.Date()) {
  cumfact <- cumprod(compound(x))
  cumterms <- term(cumsum(x@terms), units(x@terms))
  
  tf <- timefactor(x@daycount, cumterms)
  rates_ <- rates(x@compounding, tf, cumfact)
  
  spotratecurve(rates_, cumterms,
                compounding = x@compounding,
                daycount = x@daycount,
                calendar = x@calendar,
                refdate = refdate)
}

# TODO situation: given index return all NA
#      choices: define an empty spotrate object or raise an error
#' @export
setMethod(
  "[",
  signature(x = "SpotRateCurve", i = "numeric", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    obj <- spotratecurve(x@.Data[i], x@terms[i], x@compounding, x@daycount,
                         x@calendar, refdate = x@refdate)
    if (length(obj) >= 2)
      interpolation(obj) <- x@interpolation
    obj
  }
)

#' @export
setMethod(
  "[",
  signature(x = "SpotRateCurve", i = "logical", j="missing"),
  function(x, i, j, ..., drop = TRUE) {
    obj <- spotratecurve(x@.Data[i], x@terms[i], x@compounding, x@daycount,
                         x@calendar, refdate = x@refdate)
    if (length(obj) >= 2)
      interpolation(obj) <- x@interpolation
    obj
  }
)

#' @export
setMethod(
  "[",
  signature(x = "SpotRateCurve", i = "missing", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    obj <- spotratecurve(x@.Data, x@terms, x@compounding, x@daycount,
                         x@calendar, refdate = x@refdate)
    if (length(obj) >= 2)
      interpolation(obj) <- x@interpolation
    obj
  }
)

#' @export
setReplaceMethod(
  "[",
  signature(x="SpotRateCurve", i="logical", j="missing", value="numeric"),
  function(x, i, j, ..., value) {
    if (any(i > length(x@.Data)) || any(i < 1))
      stop("Index out of limits")
    x@.Data[i] <- value
    if (length(x) >= 2)
      interpolation(x) <- x@interpolation
    x
  }
)

#' @export
setReplaceMethod(
  "[",
  signature(x="SpotRateCurve", i="numeric", j="missing", value="SpotRate"),
  function(x, i, j, ..., value) {
    x[i] <- value@.Data
    if (length(x) >= 2)
      interpolation(x) <- x@interpolation
    x
  }
)

#' @export
setMethod(
  "[[",
  signature(x = "SpotRateCurve", i = "numeric", j = "missing"),
  function(x, i, j, ...) {
    if (is.null(x@interpolation)) {
      if (any(i < 0)) {
        mx <- - match(abs(i), x@terms)
        ix <- x@terms[mx]
      } else {
        mx <- match(i, x@terms)
        ix <- i
      }
      obj <- spotratecurve(x@.Data[mx], ix, x@compounding, x@daycount,
                           x@calendar, refdate = x@refdate)
    } else {
      rates_ <- interpolate(x@interpolation, i)
      obj <- spotratecurve(rates_, term(i, "days"), x@compounding, x@daycount,
                           x@calendar, refdate = x@refdate)
    }
    if (length(obj) >= 2)
      interpolation(obj) <- x@interpolation
    obj
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
    if (length(x) >= 2)
      interpolation(x) <- x@interpolation
    x
  }
)

#' @export
setReplaceMethod(
  "[[",
  signature(x="SpotRateCurve", i="numeric", j="missing", value="SpotRate"),
  function(x, i, j, ..., value) {
    warn_if_spotrate_slots_differ(x, value,
                                  "Given SpotRate objects have different slots")
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
    if (length(x) >= 2)
      interpolation(x) <- x@interpolation
    x
  }
)

#' @export
setReplaceMethod(
  "[[",
  signature(x="SpotRateCurve", i="missing", j="missing", value="SpotRateCurve"),
  function(x, i, j, ..., value) {
    warn_if_spotrate_slots_differ(x, value,
                                  "Given SpotRate objects have different slots")
    contained_from <- value@terms %in% x@terms
    contained_to <- x@terms %in% value@terms
    if (any(contained_from)) {
      x@.Data[contained_to] <- value@.Data[contained_from]
      x@terms[contained_to] <- value@terms[contained_from]
    }
    if (any(! contained_from)) {
      value_ <- c(x@.Data, value@.Data)
      terms_ <- c(x@terms, value@terms)
      ix <- order(terms_)
      x@.Data <- value_[ix]
      x@terms <- terms_[ix]
    }
    if (length(x) >= 2)
      interpolation(x) <- x@interpolation
    x
  }
)

#' @export
setMethod(
  "as.spotrate",
  signature(x = "SpotRateCurve"),
  function(x, ...) {
    spotrate(.value = x@.Data,
             compounding = x@compounding,
             daycount = x@daycount,
             calendar = x@calendar)
  }
)

#' @export
setMethod(
  "as.data.frame",
  signature(x = "SpotRateCurve"),
  function(x, row.names = NULL, optional = FALSE, ...) {
    spotrate_ <- as.spotrate(x)
    data.frame(terms = x@terms,
               dates = offset(x@refdate, x@terms, x@calendar),
               rates = spotrate_)
  }
)

#' @export
setMethod(
  "show",
  "SpotRateCurve",
  function(object) {
    hdr <- paste(as(object@compounding, "character"), as(object@daycount, "character"), object@calendar)
    
    m <- as.matrix(object@.Data, ncol=1)
    rownames(m) <- as.character(object@terms)
    colnames(m) <- "SpotRateCurve"
    print.default(head(m, 10), digits = 4)
    rem <- nrow(m) - 10
    if (rem > 0)
      cat("# ... with", rem, "more rows\n")
    cat(hdr, "\n")
    cat("Reference date:", format(object@refdate), "\n")
    if (!is.null(object@interpolation)) {
      cat("Interpolation:", as.character(object@interpolation), "\n")
    }
    invisible(object)
  }
)

#' @export
setMethod(
  "compound",
  signature(x = "SpotRateCurve", .t = "missing", .v = "missing"),
  function(x, .t, .v) {
    compound(x, x@terms)
  }
)

#' @export
setMethod(
  "c",
  signature(x = "SpotRateCurve"),
  function(x, ...) {
    dots <- list(...)
    elements <- lapply(dots, spr_builder(x))
    values_ <- c(x@.Data, unlist(lapply(elements, as.numeric)))
    rates_ <- spotrate(values_, x@compounding, x@daycount, x@calendar)
    terms_ <- c(x@terms, unlist(lapply(dots, function(dx) dx@terms)))
    obj <- spotratecurve(rates_, terms_, refdate = x@refdate)
    if (length(obj) >= 2)
      interpolation(obj) <- x@interpolation
    obj
  }
)

#' @export
maturities <- function(x) {
  df <- as.data.frame(x)
  df$dates
}

#' @export
setGeneric(
  "interpolation_error",
  function(x, ...) {
    standardGeneric("interpolation_error")
  }
)

#' @export
setMethod(
  "interpolation_error",
  signature(x = "SpotRateCurve"),
  function(x, ...) {
    interp_rates <- interpolate(x@interpolation, as.numeric(x@terms))
    sqrt(mean((interp_rates - x@.Data) ^ 2))
  }
)

