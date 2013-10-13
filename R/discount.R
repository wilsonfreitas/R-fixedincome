
#' @title discount
#' 
#' @description
#' Discount
#' 
#' @export
discount <- function(object, ...) UseMethod('discount', object)

#' @S3method discount CompoundFactor
discount.CompoundFactor <- function(cf.1, cf.2) {
    CompoundFactor(cf.2$value / cf.1$value, cf.2$term - cf.1$term)
}

