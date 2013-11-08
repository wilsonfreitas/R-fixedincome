
#' Days in base
#' 
#' Annual rates must have an amount of days to compound
#' 
#' @export dib
dib <- function(object, ...) UseMethod('dib', object)

#' @rdname dib
#' @method dib SpotRate
#' @S3method dib SpotRate
dib.SpotRate <- function (object) attr(object, 'dib')

#' @rdname dib
#' @method dib SpotRateCurve
#' @S3method dib SpotRateCurve
dib.SpotRateCurve <- function (object) attr(object, 'dib')

