
#' term
#' 
#' Given term
#' 
#' @export
term <- function(object, ...) UseMethod('term', object)

#' @rdname term
#' @method term SpotRate
#' @S3method term SpotRate
term.SpotRate <- function (object) object$term

#' @rdname term
#' @method term CompoundFactor
#' @S3method term CompoundFactor
term.CompoundFactor <- function(object) object$term

