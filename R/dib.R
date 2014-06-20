
#' Days in base
#' 
#' Annual rates must have an amount of days to compound
#' 
#' @export dib
dib <- function(object, ...) UseMethod('dib', object)

