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
#' @name rates
rates <- function(obj, ...) UseMethod('rates', obj)

