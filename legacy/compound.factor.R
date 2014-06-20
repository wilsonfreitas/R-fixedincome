#' @title compound.factor
#' 
#' @description
#' Compound factor
#' 
#' @export compound.factor
compound.factor <- function(object, ...) UseMethod('compound.factor', object)

#' @rdname compound.factor
#' @method compound.factor CompoundFactor
#' @S3method compound.factor CompoundFactor
compound.factor.CompoundFactor <- function(object) as.numeric(object)

#' @rdname compound.factor
#' @method compound.factor SpotRate
#' @S3method compound.factor SpotRate
compound.factor.SpotRate <- function (object) {
    compound.factor(as.CompoundFactor(object))
}

