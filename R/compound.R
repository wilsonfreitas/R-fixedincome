
#' @title compound
#' 
#' @description
#' Compound
#' 
#' @export compound
compound <- function(object, ...) UseMethod('compound', object)

#' @S3method compound CompoundFactor
compound.CompoundFactor <- function(cf.1, cf.2) {
    CompoundFactor(cf.1$value * cf.2$value, cf.1$term + cf.2$term)
}

