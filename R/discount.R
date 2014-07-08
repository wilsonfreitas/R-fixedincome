#' @details
#' The \code{discount} is the inverse of \code{compound}
#' \deqn{discount = \frac{1}{compound}}
#' 
#' @export
#' @rdname compound
discount <- function(obj, ...) UseMethod('discount', obj)

