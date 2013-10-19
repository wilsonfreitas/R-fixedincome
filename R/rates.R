
#' @title rates
#' 
#' @description
#' Interest rates
#' 
#' @details
#' Interest rates
#' 
#' @rdname rates
#' @export rates
rates <- function(object, ...) UseMethod('rates', object)

#' @return \code{interest rates}
#' @rdname rates
#' @method rates SpotRateCurve
#' @S3method rates SpotRateCurve
rates.SpotRateCurve <- function(object) as.numeric(object)

