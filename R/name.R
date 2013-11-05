
#' curve's name
#' 
#' \code{name} represents the curve's name
#' 
#' @export name
name <- function(object, ...) UseMethod('name', object)

#' @rdname name
#' @method   name SpotRateCurve
#' @S3method name SpotRateCurve
name.SpotRateCurve <- function (object) attr(object, 'name')

