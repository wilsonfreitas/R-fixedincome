
# .daycounts.dib <- list(
#   '30/360' = 360,
#   '30E/360' = 360,
#   'actual/365' = 365,
#   'actual/360' = 360,
#   'business/252' = 252
# )

#' @export
setClass(
  "Daycount",
  contains = "character"
)

#' @export
daycount <- function(spec, ...) {
  spec_parts = strsplit(spec, '/')[[1]]
  if (length(spec_parts) != 2)
    stop("Invalid daycount specitication provided (", spec, ")")
  new("Daycount", .Data = spec)
}

setAs(
  "Daycount", "character",
  function(from) from@.Data
)

#' @export
setGeneric(
  "dib",
  function(x) {
    standardGeneric("dib")
  }
)

#' @export
setMethod(
  "dib",
  signature(x = "Daycount"),
  function(x) {
    dc_parts <- unlist(strsplit(x@.Data, '/'))
    as.numeric(dc_parts[2])
  }
)

#' @export
setMethod(
  "show",
  signature(object = "Daycount"),
  function(object) {
    cat("<daycount:", object@.Data, "\b>", "\n")
    invisible(object)
  }
)

