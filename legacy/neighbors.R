
#' neighbors
#' 
#' Neighbors for a given term
#' 
#' @export
neighbors <- function(object, ...) UseMethod('neighbors', object)

#' @S3method neighbors SpotRateCurve
neighbors.SpotRateCurve <- function(curve, term) {
    terms(curve)[neighbors.indexes(curve, term)]
}

# return the neighbors indexes in a SpotRateCurve
neighbors.indexes <- function(curve, term) {
    c(max(which(terms(curve) <= term)), min(which(terms(curve) >= term)))
}

