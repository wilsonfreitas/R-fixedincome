
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

#' @export
spotrate <- function(.value, compounding, daycount, calendar = "actual",
                     .copyfrom = NULL) {
  if (!is.null(.copyfrom)) {
    .value <- if (missing(.value)) .copyfrom@.Data else .value
    compounding <- if (missing(compounding)) {
      .copyfrom@compounding
    } else {
      compounding
    }
    daycount <- if (missing(daycount)) .copyfrom@daycount else daycount
    calendar <- if (calendar == "actual") .copyfrom@calendar else calendar
  }

  compounding <- if (is.character(compounding)) {
    compounding(compounding)
  } else {
    compounding
  }
  daycount <- if (is.character(daycount)) daycount(daycount) else daycount
  new("SpotRate", .value,
    compounding = compounding, daycount = daycount,
    calendar = calendar
  )
}

# coercion 1: from SpotRate to ANY ----

#' @export
setMethod(
  "as.numeric",
  signature(x = "SpotRate"),
  function(x) {
    x@.Data
  }
)

#' @export
setMethod(
  "as.character",
  signature(x = "SpotRate"),
  function(x) {
    paste(
      x@.Data, as(x@compounding, "character"),
      as(x@daycount, "character"), x@calendar
    )
  }
)

#' @export
setMethod(
  "as.list",
  signature(x = "SpotRate"),
  function(x) {
    list(
      value = x@.Data,
      compounding = as(x@compounding, "character"),
      daycount = as(x@daycount, "character"),
      calendar = x@calendar
    )
  }
)

setAs(
  "SpotRate",
  "character",
  function(from) {
    as.character(from)
  }
)

setAs(
  "SpotRate",
  "numeric",
  function(from) {
    as.numeric(from)
  }
)

setAs(
  "SpotRate",
  "list",
  function(from) {
    as.list(from)
  }
)

# coercion 2: from ANY to SpotRate ----

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
  "as.spotrate",
  "list",
  function(x, ...) {
    spotrate(x$value, x$compounding, x$daycount, x$calendar)
  }
)

# print, show and format ----

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

# [spotrate], term:[numeric], units:[character]
# [spotrate], [term]
# [spotrate], from, to

# methods ----

#' @export
setGeneric(
  "discount",
  function(x, .t, .v, ...) {
    standardGeneric("discount")
  }
)

#' @export
setMethod(
  "discount",
  signature(x = "SpotRate", .t = "numeric", .v = "character"),
  function(x, .t, .v) {
    1 / compound(x, .t, .v)
  }
)

#' @export
setMethod(
  "discount",
  signature(x = "SpotRate", .t = "Term", .v = "missing"),
  function(x, .t, .v) {
    1 / compound(x, .t)
  }
)

#' @export
setMethod(
  "discount",
  signature(x = "SpotRate", .t = "Date", .v = "Date"),
  function(x, .t, .v) {
    1 / compound(x, .t, .v)
  }
)

# Ops: Arith, Compare methods ----

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

#' @export
setMethod(
  "Compare",
  signature(e1 = "SpotRate", e2 = "SpotRate"),
  function(e1, e2) {
    callGeneric(e1@.Data, e2@.Data) & check_slots(e1, e2)
  }
)

#' @export
setMethod(
  "Compare",
  signature(e1 = "SpotRate", e2 = "numeric"),
  function(e1, e2) {
    callGeneric(e1@.Data, e2)
  }
)

#' @export
setMethod(
  "Compare",
  signature(e1 = "numeric", e2 = "SpotRate"),
  function(e1, e2) {
    callGeneric(e1, e2@.Data)
  }
)

# other methods

#' @export
setMethod(
  "[",
  signature(x = "SpotRate"),
  function(x, i) {
    spotrate(x@.Data[i], x@compounding, x@daycount, x@calendar)
  }
)

#' @export
setReplaceMethod(
  "[",
  signature(x = "SpotRate"),
  function(x, i, value) {
    x@.Data[i] <- value
    x
  }
)

#' @export
setMethod(
  "length",
  signature(x = "SpotRate"),
  function(x) {
    length(x@.Data)
  }
)

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

#' @export
setMethod(
  "c",
  signature(x = "SpotRate"),
  function(x, ...) {
    dots <- list(...)
    nempty <- sapply(dots, length) != 0
    elements <- lapply(dots[nempty], spr_builder(x))
    values_ <- c(x@.Data, unlist(lapply(elements, as.numeric)))
    spotrate(values_, x@compounding, x@daycount, x@calendar)
  }
)

# convert

#' @export
setGeneric(
  "convert",
  function(x, .t1, .t2, ...) {
    standardGeneric("convert")
  }
)

#' @export
setMethod(
  "convert",
  signature(x = "SpotRate", .t1 = "Term", .t2 = "missing"),
  function(x, .t1, .t2, .compounding, .daycount, .calendar) {
    .compounding <- if (missing(.compounding)) x@compounding else .compounding
    .compounding <- if (is(.compounding, "character")) {
      compounding(.compounding)
    } else {
      .compounding
    }

    .daycount <- if (missing(.daycount)) x@daycount else .daycount
    .daycount <- if (is(.daycount, "character")) {
      daycount(.daycount)
    } else {
      .daycount
    }

    .calendar <- if (missing(.calendar)) x@calendar else .calendar

    .value <- rates(.compounding, toyears(.daycount, .t1), compound(x, .t1))
    spotrate(.value, .compounding, .daycount, .calendar)
  }
)

#' @export
setMethod(
  "convert",
  signature(x = "SpotRate", .t1 = "Date", .t2 = "Date"),
  function(x, .t1, .t2, .compounding, .daycount, .calendar) {
    .compounding <- if (missing(.compounding)) x@compounding else .compounding
    .compounding <- if (is(.compounding, "character")) {
      compounding(.compounding)
    } else {
      .compounding
    }

    .daycount <- if (missing(.daycount)) x@daycount else .daycount
    .daycount <- if (is(.daycount, "character")) {
      daycount(.daycount)
    } else {
      .daycount
    }

    .calendar <- if (missing(.calendar)) x@calendar else .calendar

    tm2 <- term(bizdays::bizdays(.t1, .t2, .calendar), "days")
    .value <- rates(
      .compounding, toyears(.daycount, tm2),
      compound(x, .t1, .t2)
    )
    spotrate(.value, .compounding, .daycount, .calendar)
  }
)

#' @export
setMethod(
  "rep",
  signature(x = "SpotRate"),
  function(x, times) {
    n <- rep(x@.Data, times)
    spotrate(n, x@compounding, x@daycount, x@calendar)
  }
)