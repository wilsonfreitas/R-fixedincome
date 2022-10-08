#' ForwardRate class
#'
#' ForwardRate class abstracts a forward rate.
#' It has an additional term, that reffers to the forward period used to
#' compute the forward rate.
#'
#' @export
setClass(
  "ForwardRate",
  slots = c(
    terms = "Term",
    refdate = "ANY"
  ),
  contains = "SpotRate"
)

#' Create a ForwardRate object
#'
#' `forwardrate()` creates a ForwardRate object.
#'
#' @param x a numeric or a SpotRateCurve object.
#' @param terms a numeric vector with positive values representing terms of
#'        the forward rates.
#' @param compounding a character with the compouning name.
#' @param daycount a character representing the daycount.
#' @param calendar a calendar object.
#' @param .copyfrom a SpotRate object that is used as reference to build
#'        the SpotRateCurve object.
#' @param refdate the curve reference date.
#' @param t1 initial term
#' @param t2 final term
#' @param ... additional arguments.
#'
#' @return A `ForwardRate` object.
#'
#' The arguments \code{t1} and \code{t2} define initial and final term used to
#' extract a ForwardRate from a SpotRateCurve.
#'
#' @export
forwardrate <- function(x, ...) {
  UseMethod("forwardrate")
}

#' @rdname forwardrate
#' @export
forwardrate.numeric <- function(x, terms, compounding, daycount,
                                calendar, .copyfrom = NULL, refdate = NULL, ...) {
  if (!is.null(.copyfrom)) {
    compounding <- if (missing(compounding)) {
      .copyfrom@compounding
    } else {
      compounding
    }
    daycount <- if (missing(daycount)) .copyfrom@daycount else daycount
    calendar <- if (missing(calendar)) .copyfrom@calendar else calendar
  }

  new("ForwardRate",
    .Data = x,
    compounding = compounding,
    daycount = daycount,
    calendar = calendar,
    terms = terms,
    refdate = refdate
  )
}

#' @rdname forwardrate
#' @export
forwardrate.SpotRateCurve <- function(x, t1 = NULL, t2 = NULL, ...) {
  if (length(x) == 1) {
    .Object <- new("ForwardRate",
      .Data = x@.Data,
      compounding = x@compounding,
      daycount = x@daycount,
      calendar = x@calendar,
      terms = x@terms,
      refdate = x@refdate
    )
  } else if (is.null(t1) && is.null(t2)) {
    factor_rate <- compound(x)
    factor_rate_b <- shift(factor_rate)
    dub <- diff(x@terms, fill = NA)
    tf <- as.numeric(toyears(x@daycount, dub))
    # first element is NA
    rates_ <- implied_rate(x@compounding, tf, factor_rate / factor_rate_b)[-1]

    .Object <- new("ForwardRate",
      .Data = c(as.numeric(x[1]), rates_),
      terms = c(x@terms[1], dub[-1]),
      compounding = x@compounding,
      daycount = x@daycount,
      calendar = x@calendar,
      refdate = x@refdate
    )
  } else {
    pos <- match(c(t1, t2), unclass(x@terms))
    fact <- compound(x)
    fact1 <- fact[pos[1]]
    fact2 <- fact[pos[2]]

    fwd_term <- t2 - t1
    tf <- as.numeric(toyears(x@daycount, fwd_term))
    rates_ <- implied_rate(x@compounding, tf, fact2 / fact1)

    .Object <- new("ForwardRate",
      .Data = rates_,
      compounding = x@compounding,
      daycount = x@daycount,
      calendar = x@calendar,
      terms = fwd_term,
      refdate = x@refdate
    )
  }
  .Object
}

#' Coerce objects to ForwardRate
#'
#' A \code{ForwardRate} object can be created from a `SpotRate` object and
#' a `SpotRateCurve`.
#'
#' @param x a `SpotRate` or a `SpotRateCurve` object.
#' @param terms a numeric with positive values representing terms or a Term
#'        object.
#' @param refdate the curve reference date.
#' @param t1 initial term
#' @param t2 final term
#' @param ... additional arguments
#'
#' @return
#' A `ForwardRate` object created from another object, `SpotRate` or
#' `SpotRateCurve`.
#'
#' @export
as.forwardrate <- function(x, ...) {
  UseMethod("as.forwardrate")
}

#' @rdname as.forwardrate
#' @export
as.forwardrate.SpotRate <- function(x, terms, refdate = NULL, ...) {
  new("ForwardRate",
    .Data = as.numeric(x),
    compounding = x@compounding,
    daycount = x@daycount,
    calendar = x@calendar,
    terms = terms,
    refdate = refdate
  )
}

#' @rdname as.forwardrate
#' @export
as.forwardrate.SpotRateCurve <- function(x, t1 = NULL, t2 = NULL, ...) {
  forwardrate.SpotRateCurve(x, t1, t2)
}

#' @export
setMethod(
  "c",
  signature(x = "ForwardRate"),
  function(x, ...) {
    dots <- list(...)
    nempty <- sapply(dots, length) != 0
    elements <- lapply(dots[nempty], spr_builder(x))
    values_ <- c(x@.Data, unlist(lapply(elements, as.numeric)))
    terms_ <- c(x@terms, unlist(lapply(dots, function(dx) dx@terms)))
    forwardrate(values_, terms_, x@compounding, x@daycount, x@calendar,
      refdate = x@refdate
    )
  }
)

#' @export
setMethod(
  "show",
  "ForwardRate",
  function(object) {
    hdr <- paste(
      as(object@compounding, "character"),
      as(object@daycount, "character"), object@calendar
    )

    m <- as.matrix(object@.Data, ncol = 1)
    .terms <- term(object@terms, "days")
    rownames(m) <- as.character(.terms)
    colnames(m) <- "ForwardRate"
    print.default(m)
    cat(hdr)
    cat("\n")
    invisible(object)
  }
)

#' @export
setMethod(
  "[",
  signature(x = "ForwardRate", i = "numeric", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    forwardrate(x@.Data[i], x@terms[i], x@compounding, x@daycount, x@calendar,
      refdate = x@refdate
    )
  }
)

#' @export
setMethod(
  "[",
  signature(x = "ForwardRate", i = "missing", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    forwardrate(x@.Data, x@terms, x@compounding, x@daycount, x@calendar,
      refdate = x@refdate
    )
  }
)

#' @export
setReplaceMethod(
  "[",
  signature(
    x = "ForwardRate", i = "numeric", j = "missing", value = "SpotRate"
  ),
  function(x, i, j, ..., value) {
    x[i] <- value@.Data
    x
  }
)

#' @export
setMethod(
  "as.data.frame",
  signature(x = "ForwardRate"),
  function(x, row.names = NULL, optional = FALSE, ...) {
    spotrate_ <- as.spotrate(x)
    if (is.null(x@refdate)) {
      data.frame(
        terms = x@terms,
        rates = spotrate_
      )
    } else {
      terms_ <- term(cumsum(x@terms), x@terms@units)
      dates_ <- offset(x@refdate, terms_, x@calendar)
      data.frame(
        terms = terms_,
        forward_terms = x@terms,
        dates = dates_,
        rates = spotrate_
      )
    }
  }
)

#' @rdname as.spotrate
#' @export
setMethod(
  "as.spotrate",
  signature(x = "ForwardRate"),
  function(x, ...) {
    spotrate(
      x = x@.Data,
      compounding = x@compounding,
      daycount = x@daycount,
      calendar = x@calendar
    )
  }
)
