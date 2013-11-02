
#' curve's reference date
#' 
#' \code{datum} represents the curve's reference date
#' 
#' @export datum
datum <- function(object, ...) UseMethod('datum', object)

#' @rdname datum
#' @method   datum SpotRateCurve
#' @S3method datum SpotRateCurve
datum.SpotRateCurve <- function (object) attr(object, 'datum')

