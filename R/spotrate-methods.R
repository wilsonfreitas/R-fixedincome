
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
