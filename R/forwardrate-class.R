

#' @export
setClass(
  "ForwardRate",
  slots = c(
    terms = "Term"
  ),
  contains = "SpotRate"
)

#' @export
forwardrate <- function(x, ...) {
  UseMethod("forwardrate")
}

#' @export
forwardrate.numeric <- function(x, terms, compounding, daycount,
                                calendar = "actual", .copyfrom = NULL) {
  if (!is.null(.copyfrom)) {
    compounding <- if (missing(compounding)) {
      .copyfrom@compounding
    } else {
      compounding
    }
    daycount <- if (missing(daycount)) .copyfrom@daycount else daycount
    calendar <- if (calendar == "actual") .copyfrom@calendar else calendar
  }

  new("ForwardRate",
    .Data = x,
    compounding = compounding,
    daycount = daycount,
    calendar = calendar,
    terms = terms
  )
}

#' @export
forwardrate.SpotRateCurve <- function(x, t1 = NULL, t2 = NULL) {
  if (length(x) == 1) {
    .Object <- new("ForwardRate",
      .Data = x@.Data,
      compounding = x@compounding,
      daycount = x@daycount,
      calendar = x@calendar,
      terms = x@terms
    )
  } else if (is.null(t1) && is.null(t2)) {
    factor_rate <- compound(x)
    factor_rate_b <- shift(factor_rate)
    dub <- diff(x@terms, fill = NA)
    tf <- toyears(x@daycount, dub)
    # first element is NA
    rates_ <- rates(x@compounding, tf, factor_rate / factor_rate_b)[-1]

    .Object <- new("ForwardRate",
      .Data = c(as.numeric(x[1]), rates_),
      terms = c(x@terms[1], dub[-1]),
      compounding = x@compounding,
      daycount = x@daycount,
      calendar = x@calendar
    )
  } else {
    pos <- match(c(t1, t2), x@terms)
    fact <- compound(x)
    fact1 <- fact[pos[1]]
    fact2 <- fact[pos[2]]

    fwd_term <- t2 - t1
    tf <- toyears(x@daycount, fwd_term)
    rates_ <- rates(x@compounding, tf, fact2 / fact1)

    .Object <- new("ForwardRate",
      .Data = rates_,
      compounding = x@compounding,
      daycount = x@daycount,
      calendar = x@calendar,
      terms = fwd_term
    )
  }
  .Object
}

#' @export
as.forwardrate <- function(x, ...) {
  UseMethod("as.forwardrate")
}

#' @export
as.forwardrate.SpotRate <- function(x, terms, ...) {
  new("ForwardRate",
    .Data = as.numeric(x),
    compounding = x@compounding,
    daycount = x@daycount,
    calendar = x@calendar,
    terms = terms
  )
}

#' @export
as.forwardrate.SpotRateCurve <- function(x, ...) {
  new("ForwardRate",
    .Data = as.numeric(x),
    compounding = x@compounding,
    daycount = x@daycount,
    calendar = x@calendar,
    terms = x@terms
  )
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
    forwardrate(values_, terms_, x@compounding, x@daycount, x@calendar)
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
    forwardrate(x@.Data[i], x@terms[i], x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setMethod(
  "[",
  signature(x = "ForwardRate", i = "missing", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    forwardrate(x@.Data, x@terms, x@compounding, x@daycount, x@calendar)
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
    data.frame(terms = x@terms, rates = spotrate_)
  }
)