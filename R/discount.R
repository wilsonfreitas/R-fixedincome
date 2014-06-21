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
discount <- function(obj, ...) UseMethod('discount', obj)

