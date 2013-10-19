#' Creates a SpotRateCurve
#' 
#' SpotRateCurve abstracts a term structure class providing methods to operate 
#' on the term structure internals.
#' 
#' Actually, the class SpotRateCurve represents a zero curve.
#' A zero curve is a set of \code{rates} indexed by \code{terms}.
#' Each pair (rate,term) is a spot rate (\code{\link{SpotRate}}) and
#' it defines the cost of a loan maturing in the term.
#' Once a SpotRateCurve is created, any rate can be retrieved by its term 
#' through the index operator (\code{`[`}) which returns a \code{\link{SpotRate}}.
#' For those terms which don't have a corresponding rate, the interpolation 
#' method is used to generate a rate.
#' The parameter \code{interp} specifies the interpolation method to use.
#' Since the operator \code{`[`} returns a SpotRate the \code{dib} must be
#' provided to generate spot rates accordingly.
#'
#' @param rates a vector with the interest rates
#' @param terms a vector with the terms
#' @param interp defines which interpolation method to use: 
#' Linear, FlatForward, LogLinear, Hermite Cubic Spline, Monotone Cubic Spline,
#' Natural Cubi Spline
#' @return SpotRateCurve object.
#' @export
#' @examples
#' # Creating a zero curve 
#' curve <- SpotRateCurve(c(0.08, 0.083, 0.089, 0.093, 0.095), c(0.5, 1, 1.5, 2, 2.5))
#' # Creating a zero curve with linear interpolation and 360 days in base (days per year)
#' curve <- SpotRateCurve(c(0.08, 0.083, 0.089, 0.093, 0.095), c(0.5, 1, 1.5, 2, 2.5), dib=360, compounding='simple')
SpotRateCurve <- function(rates, terms, dib=252, compounding='compounded') {
    stopifnot(length(rates) == length(terms))
    stopifnot(length(rates) > 2)
    stopifnot(length(terms) == length(unique(terms)))
    stopifnot(all(diff(terms) > 0))
    dim(rates) <- c(length(rates), 1)
    rownames(rates) <- terms
    attr(rates, 'dib') <- dib
    attr(rates, 'compounding') <- compounding
    class(rates) <- 'SpotRateCurve'
    invisible(rates)
}

#' Coerce objects to SpotRateCurve
#' 
#' A SpotRateCurve can be coerced from \code{matrix} and \code{data.frame}.
#'
#' @param object should be a \code{matrix} or a \code{data.frame}
#' @param ... other SpotRateCurve's parameters
#' @return SpotRateCurve object
#' @rdname as.SpotRateCurve
#' @export as.SpotRateCurve
as.SpotRateCurve <- function(object, ...) UseMethod('as.SpotRateCurve', object)

#' @return SpotRateCurve object
#' 
#' @rdname as.SpotRateCurve
#' @method as.SpotRateCurve data.frame
#' @S3method as.SpotRateCurve data.frame
#' @examples
#' # creating a SpotRateCurve from a data.frame
#' df <- data.frame(rates=c(0.08, 0.083, 0.089, 0.093, 0.095), terms=c(0.5, 1, 1.5, 2, 2.5))
#' curve <- as.SpotRateCurve(df, interp='Linear', dib=360)
as.SpotRateCurve.data.frame <- function(object, ...) {
    SpotRateCurve(rates=object[,1], terms=object[,2], ...)
}

#' @return SpotRateCurve object
#' 
#' @rdname as.SpotRateCurve
#' @method as.SpotRateCurve data.frame
#' @S3method as.SpotRateCurve data.frame
#' @examples
#' # creating a SpotRateCurve from a matrix
#' mat <- cbind(c(0.08, 0.083, 0.089, 0.093, 0.095), c(0.5, 1, 1.5, 2, 2.5))
#' curve <- as.SpotRateCurve(mat, interp='Spline', dib=365)
as.SpotRateCurve.matrix <- function(object, ...) {
    SpotRateCurve(rates=object[,1], terms=object[,2], ...)
}

#' @rdname open-brace.SpotRateCurve
#' @method [ SpotRateCurve
#' @S3method [ SpotRateCurve
'[.SpotRateCurve' <- function(object, term) {
    stopifnot(any(terms(object) %in% abs(term)))
    if (any(term > 0)) {
        rates(object)[terms(object) %in% term]
    } else {
        term <- abs(term)
        idx <- which(terms(object) %in% term)
        rates(object)[-idx]
    }
}

#' Get the SpotRate for a given term
#' 
#' Get the SpotRate for a given term
#' 
#' @export
getSpotRate <- function(curve, term) {
    SpotRate(curve[term], term, dib=dib(curve), compounding=compounding(curve))
}

#' Insert a SpotRate into the SpotRate.
#'
#' A SpotRateCurve can be expanded by inserting other SpotRate objects into it.
#'
#' @rdname open-brace.SpotRateCurve
#' @method [<- SpotRateCurve
#' @S3method [<- SpotRateCurve
#' @param object SpotRateCurve on which the SpotRate has to be inserted
#' @param value SpotRate to be inserted
#' @examples
#' # creating a SpotRateCurve from a matrix
#' mat <- cbind(c(0.08, 0.083, 0.089, 0.093, 0.095), c(0.5, 1, 1.5, 2, 2.5))
#' curve <- as.SpotRateCurve(mat, interp='Spline', dib=365)
#' curve[1.25] <- 0.085
'[<-.SpotRateCurve' <- function(object, i, value) {
    terms <- terms(object)
    rates <- rates(object)
    contained.idx <- i %in% terms
    if (any(contained.idx)) {
        idx <- terms %in% i
        terms[idx] <- i[contained.idx]
        rates[idx] <- value[contained.idx]
    }
    if (any(!contained.idx)) {
        rates <- append(rates, value[!contained.idx])
        terms <- append(terms, i[!contained.idx])
        idx <- order(terms)
        terms <- terms[idx]
        rates <- rates[idx]
    }
    SpotRateCurve(rates, terms, dib(object), compounding(object))
}

#' SpotRateCurve generic extensions
#' 
#' SpotRateCurve generic extensions
#' 
#' @rdname generic-SpotRateCurve
#' @name generic-SpotRateCurve
NULL

#' @rdname generic-SpotRateCurve
#' @method as.data.frame SpotRateCurve
#' @S3method as.data.frame SpotRateCurve
as.data.frame.SpotRateCurve <- function(curve, ...) {
    data.frame(terms=terms(curve), rates=rates(curve), ...)
}

#' @rdname generic-SpotRateCurve
#' @method length SpotRateCurve
#' @S3method length SpotRateCurve
length.SpotRateCurve <- function(curve) dim(curve)[1]

#' @rdname generic-SpotRateCurve
#' @method plot SpotRateCurve
#' @S3method plot SpotRateCurve
plot.SpotRateCurve <- function(curve, ...) {
    plot(terms(curve), rates(curve), ...)
}

