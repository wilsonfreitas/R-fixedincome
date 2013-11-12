
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

#' @return \code{interest rates}
#' @rdname rates
#' @method rates CurveInterpolation
#' @S3method rates CurveInterpolation
rates.CurveInterpolation <- function(object, terms=NULL) {
	if (is.null(terms))
		NextMethod('rates', object)
	else {
		interp <- attr(object, 'interp')
		interp(terms)
	}
}

