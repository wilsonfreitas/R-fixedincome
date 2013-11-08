
#' @title discount
#' 
#' @description
#' Discount
#' 
#' @export
discount <- function(object, ...) UseMethod('discount', object)

#' @S3method discount CompoundFactor
discount.CompoundFactor <- function(cf.1, cf.2) {
    CompoundFactor(compound.factor(cf.2) / compound.factor(cf.1),
        term(cf.2) - term(cf.1))
}

