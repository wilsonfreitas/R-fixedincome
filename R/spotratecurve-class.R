#' SpotRateCurve class
#'
#' The SpotRateCurve class abstracts a term structure of SpotRate objects.
#' The SpotRateCurve has a reference date (\code{refdate} slot), that is a
#' mark to market date.
#' The SpotRates are indexed to future dates according to its reference date
#' and these future dates represent the terms of the SpotRateCurve.
#'
#' Once the SpotRateCurve object is built, any SpotRate can be accessed
#' using indexing operations: \code{[]} positional indexing, \code{[[]]}
#' term indexing.
#'
#' The SpotRateCurve inherits SpotRate class and has three slots:
#' terms that is a Term object, refdate and interpolation that defines the
#' method used to interpolate the curve.
#'
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
    terms_na_check <- !any(is.na(object@terms))
    len_check & unique_check & terms_positivity_check & terms_na_check
  },
  contains = "SpotRate"
)

#' Create a SpotRateCurve object
#'
#' `spotratecurve()` S3 method createas a SpotRateCurve object.
#' It is dispatched for numeric values, that represent spot rates and
#' for SpotRate objects.
#'
#' @param x a numeric representing a spot rate value or a SpotRate object.
#' @param terms a numeric vector with positive values representing the days of
#'        the term structure.
#' @param compounding a character with the compouning name.
#' @param daycount a character representing the daycount.
#' @param calendar a calendar object.
#' @param refdate the curve reference date.
#' @param .copyfrom a SpotRate object that is used as reference to build
#'        the SpotRateCurve object.
#' @param ... additional arguments
#'
#' @return A `SpotRateCurve` object.
#'
#' @examples
#' terms <- c(1, 11, 26, 27, 28)
#' rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
#'
#' curve <- spotratecurve(rates, terms, "discrete", "actual/365", "actual")
#'
#' # access the term 11 days
#' curve[[11]]
#'
#' # access the second element
#' curve[2]
#' @export
spotratecurve <- function(x, terms, ..., refdate = Sys.Date()) {
  UseMethod("spotratecurve")
}

#' @rdname spotratecurve
#' @export
spotratecurve.numeric <- function(x, terms, compounding, daycount,
                                  calendar, refdate = Sys.Date(),
                                  .copyfrom = NULL, ...) {
  .underlying <- spotrate(
    x,
    compounding = compounding,
    daycount = daycount,
    calendar = calendar,
    .copyfrom = .copyfrom
  )

  .Object <- new("SpotRateCurve",
    .Data = .underlying@.Data,
    compounding = .underlying@compounding,
    daycount = .underlying@daycount,
    calendar = .underlying@calendar,
    terms = terms,
    refdate = refdate
  )

  ix <- order(terms)
  .Object@terms <- term(terms[ix], "days")
  .Object@.Data <- .Object@.Data[ix]
  validObject(.Object)
  .Object
}

#' @rdname spotratecurve
#' @export
spotratecurve.SpotRate <- function(x, terms,
                                   refdate = Sys.Date(),
                                   .copyfrom = NULL, ...) {
  .Object <- new("SpotRateCurve",
    .Data = x@.Data,
    compounding = x@compounding,
    daycount = x@daycount,
    calendar = x@calendar,
    terms = terms,
    refdate = refdate
  )

  ix <- order(terms)
  .Object@terms <- term(terms[ix], "days")
  .Object@.Data <- .Object@.Data[ix]
  validObject(.Object)
  .Object
}

#' Coerce objects to spotratecurve
#'
#' A SpotRateCurve can be created from a \code{ForwardRate} object.
#'
#' @param x a ForwardRate object.
#' @param refdate the curve reference date.
#' @param ... additional arguments
#'
#' @return A `SpotRateCurve` object create from another object.
#'
#' @export
as.spotratecurve <- function(x, ...) {
  UseMethod("as.spotratecurve")
}

#' @rdname as.spotratecurve
#' @export
as.spotratecurve.ForwardRate <- function(x, refdate = Sys.Date(), ...) {
  cumfact <- cumprod(compound(x))
  cumterms <- term(cumsum(x@terms), x@terms@units)

  tf <- toyears(x@daycount, cumterms)
  rates_ <- rates(x@compounding, tf, cumfact)

  spotratecurve(rates_, cumterms,
    compounding = x@compounding,
    daycount = x@daycount,
    calendar = x@calendar,
    refdate = refdate
  )
}

# TODO situation: given index return all NA
#      choices: define an empty spotrate object or raise an error
#' @export
setMethod(
  "[",
  signature(x = "SpotRateCurve", i = "numeric", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    obj <- spotratecurve(x@.Data[i], x@terms[i], x@compounding, x@daycount,
      x@calendar,
      refdate = x@refdate
    )
    interpolation(obj) <- x@interpolation
    obj
  }
)

#' @export
setMethod(
  "[",
  signature(x = "SpotRateCurve", i = "logical", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    obj <- spotratecurve(x@.Data[i], x@terms[i], x@compounding, x@daycount,
      x@calendar,
      refdate = x@refdate
    )
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
      x@calendar,
      refdate = x@refdate
    )
    interpolation(obj) <- x@interpolation
    obj
  }
)

#' @export
setReplaceMethod(
  "[",
  signature(
    x = "SpotRateCurve", i = "logical", j = "missing",
    value = "numeric"
  ),
  function(x, i, j, ..., value) {
    if (any(i > length(x@.Data)) || any(i < 1)) {
      stop("Index out of limits")
    }
    x@.Data[i] <- value
    interpolation(x) <- x@interpolation
    x
  }
)

#' @export
setReplaceMethod(
  "[",
  signature(
    x = "SpotRateCurve", i = "numeric", j = "missing",
    value = "SpotRate"
  ),
  function(x, i, j, ..., value) {
    x[i] <- value@.Data
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
        mx <- -match(abs(i), x@terms)
        ix <- x@terms[mx]
      } else {
        mx <- match(i, x@terms)
        ix <- i
      }
      obj <- spotratecurve(x@.Data[mx], ix, x@compounding, x@daycount,
        x@calendar,
        refdate = x@refdate
      )
    } else {
      rates_ <- interpolate(x@interpolation, i)
      obj <- spotratecurve(rates_, term(i, "days"), x@compounding, x@daycount,
        x@calendar,
        refdate = x@refdate
      )
    }
    if (!is.null(x@interpolation) &&
      x@interpolation@propagate) {
      interpolation(obj) <- x@interpolation
    }
    obj
  }
)

replace_double_brackets <- function(x, i, value) {
  contained_from <- i %in% x@terms
  contained_to <- x@terms %in% i
  if (any(contained_from)) {
    x@.Data[contained_to] <- if (length(value) == 1) {
      value
    } else {
      value[contained_from]
    }
  }
  if (any(!contained_from)) {
    value_ <- c(x@.Data, if (length(value) == 1) {
      rep(value, sum(!contained_from))
    } else {
      value[!contained_from]
    })
    terms_ <- c(x@terms, i[!contained_from])
    ix <- order(terms_)
    x@.Data <- value_[ix]
    x@terms <- terms_[ix]
  }
  if (length(x) >= 2) {
    interpolation(x) <- x@interpolation
  }
  x
}

#' @export
setReplaceMethod(
  "[[",
  signature(
    x = "SpotRateCurve", i = "numeric", j = "missing",
    value = "numeric"
  ),
  function(x, i, j, ..., value) {
    replace_double_brackets(x, i, value)
  }
)

#' @export
setReplaceMethod(
  "[[",
  signature(
    x = "SpotRateCurve", i = "numeric", j = "missing",
    value = "SpotRate"
  ),
  function(x, i, j, ..., value) {
    warn_if_spotrate_slots_differ(
      x, value,
      "Given SpotRate objects have different slots"
    )
    replace_double_brackets(x, i, value)
  }
)

#' @export
setReplaceMethod(
  "[[",
  signature(
    x = "SpotRateCurve", i = "missing", j = "missing",
    value = "SpotRateCurve"
  ),
  function(x, i, j, ..., value) {
    warn_if_spotrate_slots_differ(
      x, value,
      "Given SpotRate objects have different slots"
    )
    contained_from <- value@terms %in% x@terms
    contained_to <- x@terms %in% value@terms
    if (any(contained_from)) {
      x@.Data[contained_to] <- value@.Data[contained_from]
      x@terms[contained_to] <- value@terms[contained_from]
    }
    if (any(!contained_from)) {
      value_ <- c(x@.Data, value@.Data)
      terms_ <- c(x@terms, value@terms)
      ix <- order(terms_)
      x@.Data <- value_[ix]
      x@terms <- terms_[ix]
    }
    if (length(x) >= 2) {
      interpolation(x) <- x@interpolation
    }
    x
  }
)

#' @rdname as.spotrate
#' @export
setMethod(
  "as.spotrate",
  signature(x = "SpotRateCurve"),
  function(x, ...) {
    spotrate(
      x = x@.Data,
      compounding = x@compounding,
      daycount = x@daycount,
      calendar = x@calendar
    )
  }
)

#' @export
setMethod(
  "as.data.frame",
  signature(x = "SpotRateCurve"),
  function(x, row.names = NULL, optional = FALSE, ...) {
    spotrate_ <- as.spotrate(x)
    data.frame(
      terms = x@terms,
      dates = offset(x@refdate, x@terms, x@calendar),
      rates = spotrate_
    )
  }
)

#' @export
setMethod(
  "show",
  "SpotRateCurve",
  function(object) {
    hdr <- paste(
      as(object@compounding, "character"),
      as(object@daycount, "character"), object@calendar
    )

    m <- as.matrix(object@.Data, ncol = 1)
    rownames(m) <- as.character(object@terms)
    colnames(m) <- "SpotRateCurve"
    print.default(head(m, 10), digits = 4)
    rem <- nrow(m) - 10
    if (rem > 0) {
      cat("# ... with", rem, "more rows\n")
    }
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
  "c",
  signature(x = "SpotRateCurve"),
  function(x, ...) {
    dots <- list(...)
    elements <- lapply(dots, spr_builder(x))
    values_ <- c(x@.Data, unlist(lapply(elements, as.numeric)))
    rates_ <- spotrate(values_, x@compounding, x@daycount, x@calendar)
    terms_ <- c(x@terms, unlist(lapply(dots, function(dx) dx@terms)))
    obj <- spotratecurve(rates_, terms_, refdate = x@refdate)
    if (length(obj) >= 2) {
      interpolation(obj) <- x@interpolation
    }
    obj
  }
)

#' Get SpotRateCurve terms as Date objects
#'
#' Compute the SpotRateCurve terms as Date objects, according to the curve's
#' reference date.
#'
#' @param x a SpotRateCurve object.
#'
#' @return
#' A vector of `Date` objects that represent the curve's `terms` and using
#' curve's `refdate` as a starting point.
#'
#' @examples
#' terms <- c(1, 11, 26, 27, 28)
#' rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
#' curve <- spotratecurve(rates, terms, "discrete", "actual/365", "actual")
#' maturities(curve)
#' @export
maturities <- function(x) {
  df <- as.data.frame(x)
  df$dates
}

#' Interpolation error
#'
#' Computes interpolation error as the root mean square error of differences
#' between interpolated terms and SpotRateCurve values.
#'
#' @param x a SpotRateCurve object.
#' @param ... additional arguments. Currently unused.
#'
#' The curve must have a interpolation set to compute the interpolation error.
#' This is useful to evaluate parametric methods like [NelsonSiegel-class] and
#' [NelsonSiegelSvensson-class].
#'
#' @return
#' A numeric value with the root mean squared error between the curve data point
#' and interpolated points.
#'
#' @aliases interpolation_error,SpotRateCurve-method
#' @examples
#' terms <- c(1, 11, 26, 27, 28)
#' rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
#' curve <- spotratecurve(rates, terms, "discrete", "actual/365", "actual")
#' interpolation(curve) <- interp_nelsonsiegel(
#'   0.1229, -0.0606, 0.1004, 1.9174
#' )
#' interpolation_error(curve)
#' @export
setGeneric(
  "interpolation_error",
  function(x, ...) {
    standardGeneric("interpolation_error")
  }
)

setMethod(
  "interpolation_error",
  signature(x = "SpotRateCurve"),
  function(x, ...) {
    interp_rates <- interpolate(x@interpolation, as.numeric(x@terms))
    sqrt(mean((interp_rates - x@.Data)^2))
  }
)

#' SpotRateCurve helpers
#'
#' Helpers methods that return parts of a SpotRateCurve object according to a
#' given term.
#'
#' @param x a SpotRateCurve object.
#' @param t a Term object.
#'
#' `first` filters the first elements of the SpotRateCurve according to the
#' given term.
#'
#' `last` filters the last elements of the SpotRateCurve according to the
#' given term.
#'
#' `closest` selects the element of the SpotRateCurve that is the closest to
#' the given term.
#'
#' @name spotratecurve-helpers
#' @return
#' A `SpotRateCurve` object that is a subset of the given curve.
#' The elements returned are select according to the operation executed.
#' @aliases
#' closest,SpotRateCurve,Term-method
#' closest,SpotRateCurve,character-method
#' first,SpotRateCurve,Term-method
#' first,SpotRateCurve,character-method
#' last,SpotRateCurve,Term-method
#' last,SpotRateCurve,character-method
#' @examples
#' terms <- c(1, 11, 26, 27, 28)
#' rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
#' curve <- spotratecurve(rates, terms, "discrete", "actual/365", "actual")
#' first(curve, "10 days")
#' last(curve, "10 days")
#' closest(curve, "10 days")
NULL

#' @rdname spotratecurve-helpers
#' @export
setGeneric(
  "first",
  function(x, t) {
    standardGeneric("first")
  }
)

setMethod(
  "first",
  signature(x = "SpotRateCurve", t = "Term"),
  function(x, t) {
    idx <- toyears(x@daycount, x@terms) <= toyears(x@daycount, t)
    x[idx]
  }
)

setMethod(
  "first",
  signature(x = "SpotRateCurve", t = "character"),
  function(x, t) {
    first(x, as.term(t))
  }
)

#' @rdname spotratecurve-helpers
#' @export
setGeneric(
  "last",
  function(x, t) {
    standardGeneric("last")
  }
)

setMethod(
  "last",
  signature(x = "SpotRateCurve", t = "Term"),
  function(x, t) {
    t_tf <- toyears(x@daycount, t)
    x_tf <- toyears(x@daycount, x@terms)
    limit_tf <- max(x_tf) - t_tf
    idx <- toyears(x@daycount, x@terms) >= limit_tf
    x[idx]
  }
)

setMethod(
  "last",
  signature(x = "SpotRateCurve", t = "character"),
  function(x, t) {
    last(x, as.term(t))
  }
)

#' @rdname spotratecurve-helpers
#' @export
setGeneric(
  "closest",
  function(x, t) {
    standardGeneric("closest")
  }
)

setMethod(
  "closest",
  signature(x = "SpotRateCurve", t = "Term"),
  function(x, t) {
    t_tf <- toyears(x@daycount, t)
    x_tf <- toyears(x@daycount, x@terms)
    x[which.min(abs(x_tf - t_tf))]
  }
)

setMethod(
  "closest",
  signature(x = "SpotRateCurve", t = "character"),
  function(x, t) {
    closest(x, as.term(t))
  }
)