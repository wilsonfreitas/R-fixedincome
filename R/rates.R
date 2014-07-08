#' Return the numeric rates
#' 
#' Return a numeric value which represents spot rates.
#' 
#' @param obj See Details
#' @param ... extra arguments
#' 
#' @return a numeric value
#' 
#' @export
#' @name rates
rates <- function(obj, ...) UseMethod('rates', obj)

