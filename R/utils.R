#' Shift vectors
#'
#' Element wise shift of vectors by `k` positions.
#'
#' @param x a vector object.
#' @param k a numeric with the number of elements to shift the Term vector
#' @param fill a numeric value (or \code{NA}) to fill the empty created by
#' shifting a vector object.
#' @param ... additional arguments. Currently unused.
#'
#' @return
#' A shifted vector object of the same type of provided object.
#'
#' @aliases shift,Term-method shift,numeric-method
#' @examples
#' shift(1:10, fill = 0)
#'
#' t <- term(1:10, "months")
#' shift(t)
#' @export
setGeneric(
  "shift",
  function(x, k = 1, ..., fill = NA) {
    standardGeneric("shift")
  }
)

setMethod(
  "shift",
  signature(x = "numeric"),
  function(x, k = 1, ..., fill = NA) {
    stopifnot(length(x) >= k)
    if (k == 0) {
      return(x)
    }

    forward <- k > 0
    k <- abs(k)

    if (forward) {
      shifted <- c(rep(fill, k), x[seq_len(length(x) - k)])
    } else {
      shifted <- c(x[seq_len(length(x) - k) + k], rep(fill, k))
    }

    shifted
  }
)