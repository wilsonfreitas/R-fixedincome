
#' @export
setGeneric(
  "shift",
  function(x, k = 1, ...) {
    standardGeneric("shift")
  }
)

#' @export
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
      shifted <- c(x[seq_len(length(x)-k)+k], rep(fill, k))
    }
    
    shifted
  }
)

