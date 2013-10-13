
#' terms
#' 
#' Given terms
#' 
#' @export
terms <- function(object, ...) UseMethod('terms', object)

#' @S3method terms SpotRateCurve
terms.SpotRateCurve <- function(object) object$terms
