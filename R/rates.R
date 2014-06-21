#' Compound function
#' 
#' Computes the compounding factor
#' 
#' Compounding factor
#' 
#' @param obj .
#' @param ... .
#' 
#' @export
rates <- function(obj, ...) UseMethod('rates', obj)

