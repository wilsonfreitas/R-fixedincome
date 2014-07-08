#' Calendar accessor
#' 
#' Method to access the \code{calendar} attribute.
#' 
#' @param obj any object which has a calendar attribute
#' @param ... extra arguments
#' 
#' @return a \code{calendar} object
#' 
#' @export
calendar <- function(obj, ...) UseMethod('calendar', obj)

