
#' @title dib
#' 
#' @description
#' Days in base
#' 
#' @export dib
dib <- function(object, ...) UseMethod('dib', object)

#' @rdname dib
#' @method dib SpotRate
#' @S3method dib SpotRate
dib.SpotRate <- function (object) object$dib

#' @rdname dib
#' @method dib SpotRateCurve
#' @S3method dib SpotRateCurve
dib.SpotRateCurve <- function (object) attr(object, 'dib')

