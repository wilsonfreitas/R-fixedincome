#' Compound function
#' 
#' Computes the compounding factor
#' 
#' @param obj See Details
#' @param term See Details
#' @param ... extra arguments
#' 
#' @return a numeric value
#' 
#' @export
compound <- function(obj, ...) UseMethod('compound', obj)

