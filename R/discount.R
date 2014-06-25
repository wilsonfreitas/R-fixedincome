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
#' @name discount
discount <- function(obj, ...) UseMethod('discount', obj)

