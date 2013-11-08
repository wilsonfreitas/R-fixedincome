
#' @title compound
#' 
#' @description
#' Compound
#' 
#' @export compound
compound <- function(object, ...) UseMethod('compound', object)

#' @S3method compound CompoundFactor
compound.CompoundFactor <- function(cf.1, cf.2) {
    CompoundFactor(compound.factor(cf.1) * compound.factor(cf.2),
        term(cf.1) + term(cf.2))
}

