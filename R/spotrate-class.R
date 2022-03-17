#' SpotRate class
#'
#' @description
#' The \code{SpotRate} class abstracts the interst rate and has methods
#' to handle many calculations on it.
#'
#' @note
#' The \code{SpotRate} objects are annual rates.
#'
#' @details
#' The \code{SpotRate} class fully specifies spot rates.
#' It has:
#' \itemize{
#'   \item the spot rate values which are numeric values representing the rate.
#'   \item the compounding regime that specifies how to compound the spot
#'         rate. This is a \code{Compounding} object.
#'   \item the daycount rule to compute the compounding periods right
#'         adjusted to the spot rate frequency (which is annual).
#'   \item the calendar according to which the number of days are counted.
#' }
#'
#' The \code{SpotRate} class is a \code{numeric}, that represents the
#' interest rate and that has the slots: \code{compounding}, \code{daycount}
#' and \code{calendar}.
#'
#' The \code{calendar} slot is a \code{bizdays} calendar.
#'
#' @export
setClass(
  "SpotRate",
  slots = c(
    compounding = "Compounding",
    daycount = "Daycount",
    calendar = "character"
  ),
  contains = "numeric"
)

#' Create SpotRate objects
#'
#' @description
#' `spotrate()` function creates `SpotRate` objects.
#'
#' @param x a numeric vector representing spot rate values.
#' @param compounding a \code{Compounding} object.
#' @param daycount a \code{Daycount} object.
#' @param calendar a \code{bizdays} calendar.
#' @param .copyfrom a \code{SpotRate} object used as reference to copy
#'        attributes.
#' 
#' @return A `SpotRate` object.
#'
#' @examples
#' spotrate(0.06, "continuous", "actual/365", "actual")
#' spotrate(c(0.06, 0.07, 0.08), "continuous", "actual/365", "actual")
#' @export
spotrate <- function(x, compounding, daycount, calendar, .copyfrom = NULL) {
  if (!is.null(.copyfrom)) {
    x <- if (missing(x)) .copyfrom@.Data else x
    compounding <- if (missing(compounding)) {
      .copyfrom@compounding
    } else {
      compounding
    }
    daycount <- if (missing(daycount)) .copyfrom@daycount else daycount
    calendar <- if (missing(calendar)) .copyfrom@calendar else calendar
  }

  compounding <- if (is.character(compounding)) {
    compounding(compounding)
  } else {
    compounding
  }
  daycount <- if (is.character(daycount)) daycount(daycount) else daycount
  new("SpotRate", x,
    compounding = compounding, daycount = daycount,
    calendar = calendar
  )
}

#' @export
as.character.SpotRate <- function(x, ...) {
  paste(
    x@.Data, as(x@compounding, "character"),
    as(x@daycount, "character"), x@calendar
  )
}

#' @export
format.SpotRate <- function(x, ...) {
  hdr <- paste(
    as(x@compounding, "character"),
    as(x@daycount, "character"), x@calendar
  )
  paste(callGeneric(x@.Data, ...), hdr)
}

#' @export
setMethod(
  "show",
  signature(object = "SpotRate"),
  function(object) {
    print(format(object))
  }
)

#' @export
`[.SpotRate` <- function(x, i) {
  spotrate(x@.Data[i], x@compounding, x@daycount, x@calendar)
}

#' @export
`[<-.SpotRate` <- function(x, i, value) {
  x@.Data[i] <- value
  x
}

#' @export
c.SpotRate <- function(x, ...) {
  dots <- list(...)
  nempty <- sapply(dots, length) != 0
  elements <- lapply(dots[nempty], spr_builder(x))
  values_ <- c(x@.Data, unlist(lapply(elements, as.numeric)))
  spotrate(values_, x@compounding, x@daycount, x@calendar)
}

#' Coerce to SpotRate
#'
#' @description
#' Coerce character objects to SpotRate class
#'
#' @param x a character with SpotRate specification.
#' @param simplify a boolean indicating whether to simplify SpotRate creation
#'        or not. Defaults to TRUE.
#' @param ... additional arguments
#'
#' @details
#'
#' The character representation of a SpotRate is as follows:
#'
#' \preformatted{"RATE COMPOUNDING DAYCOUNT CALENDAR"}
#'
#' where:
#' \itemize{
#'   \item \code{RATE} is a numeric value
#'   \item \code{COMPOUNDING} is one of the following:
#'         \code{simple}, \code{discrete}, \code{continuous}
#'   \item \code{DAYCOUNT} is a valid day count rule, pex. \code{business/252},
#'         see [Daycount-class].
#'   \item \code{CALENDAR} is the name of a bizdays calendar.
#' }
#'
#' \code{simplify} check if compounding, daycount and calendar are the same for
#' all given characters.
#' If it is true the returned object is a SpotRate otherwise a \code{list} with
#' SpotRate objects is returned.
#' 
#' @return A `SpotRate` object created from a string.
#'
#' @examples
#'
#' as.spotrate(c(
#'   "0.06 simple actual/365 actual",
#'   "0.11 discrete business/252 actual"
#' ))
#' @export
setGeneric(
  "as.spotrate",
  function(x, ...) {
    standardGeneric("as.spotrate")
  }
)

.parse_spotrate <- function(x) {
  lapply(strsplit(x, "\\s+", perl = TRUE), function(x) {
    if (length(x) != 4) {
      stop("Invalid spotrate specification")
    }
    spotrate(as.numeric(x[1]), x[2], x[3], x[4])
  })
}

#' @rdname as.spotrate
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
        specs <- strsplit(specs, "\\s+")[[1]]
        if (length(specs) != 3) {
          stop("Invalid spotrate specification")
        }
        spotrate(value, specs[1], specs[2], specs[3])
      } else {
        .parse_spotrate(x)
      }
    } else {
      .parse_spotrate(x)
    }
  }
)

#' @export
setMethod(
  "Arith",
  signature(e1 = "SpotRate", e2 = "SpotRate"),
  function(e1, e2) {
    e1@.Data <- callGeneric(e1@.Data, e2@.Data)
    warn_if_spotrate_slots_differ(
      e1, e2,
      "Arith operation with SpotRate classes that have different slots"
    )
    e1
  }
)

#' @export
setMethod(
  "Arith",
  signature(e1 = "SpotRate", e2 = "numeric"),
  function(e1, e2) {
    e1@.Data <- callGeneric(e1@.Data, e2)
    e1
  }
)

#' @export
setMethod(
  "Arith",
  signature(e1 = "numeric", e2 = "SpotRate"),
  function(e1, e2) {
    e2@.Data <- callGeneric(e1, e2@.Data)
    e2
  }
)

#' SpotRate comparison operations
#'
#' Comparison operations with SpotRate class
#' \code{SpotRate} objects can be compared among themselves or with numeric
#' variables.
#'
#' @param e1 a \code{SpotRate} object or a numeric
#' @param e2 a \code{SpotRate} object or a numeric
#'
#' @return
#' A boolean `logical` object.
#' The comparison with `SpotRate` objects only takes all fields
#' into account.
#' Comparing `SpotRate` against numeric values is equivalent to
#' coerce the `SpotRate` object to numeric execute the operation,
#' this is a syntax sugar for a shortcut that is commonly applied.
#' @name spotrate-compare-method
#'
#' @examples
#'
#' spr <- as.spotrate("0.06 simple actual/365 actual")
#' spr == 0.06
NULL

#' @rdname spotrate-compare-method
#' @export
setMethod(
  "Compare",
  signature(e1 = "SpotRate", e2 = "SpotRate"),
  function(e1, e2) {
    callGeneric(e1@.Data, e2@.Data) & check_slots(e1, e2)
  }
)

#' @rdname spotrate-compare-method
#' @export
setMethod(
  "Compare",
  signature(e1 = "SpotRate", e2 = "numeric"),
  function(e1, e2) {
    callGeneric(e1@.Data, e2)
  }
)

#' @rdname spotrate-compare-method
#' @export
setMethod(
  "Compare",
  signature(e1 = "numeric", e2 = "SpotRate"),
  function(e1, e2) {
    callGeneric(e1, e2@.Data)
  }
)

# support functions ----

check_slots <- function(e1, e2) {
  (e1@compounding == e2@compounding) &
    (e1@daycount == e2@daycount) &
    (e1@calendar == e2@calendar)
}

warn_if_spotrate_slots_differ <- function(e1, e2, msg) {
  if (!check_slots(e1, e2)) {
    warning(msg)
  }
}

stop_if_spotrate_slots_differ <- function(e1, e2, msg) {
  if (!check_slots(e1, e2)) {
    stop(msg)
  }
}

spr_builder <- function(x) {
  function(values_) {
    if (is(values_, "SpotRate")) {
      warn_if_spotrate_slots_differ(
        x,
        values_,
        "Given SpotRate has different slots. This is ignored in concatenation"
      )
      values_ <- as.numeric(values_)
    }
    spotrate(values_, x@compounding, x@daycount, x@calendar)
  }
}