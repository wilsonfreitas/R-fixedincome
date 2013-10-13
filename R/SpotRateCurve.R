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
#' curve <- SpotRateCurve(c(0.08, 0.083, 0.089, 0.093, 0.095), c(0.5, 1, 1.5, 2, 2.5), interp='Linear', dib=360)
SpotRateCurve <- function(rates, terms, dib=252, compounding='compounded') {

    that <- list()
    stopifnot(length(rates) == length(terms))
    stopifnot(length(rates) > 2)
    that$rates <- rates
    that$terms <- terms
    that$dib <- dib
    # that$interp.method <- interp
    # that$interp.FUN <- eval(parse(text=paste('interp.', interp, sep='')))
    # that$interp.FUN2 <- eval(parse(text=paste('interp.', interp, '.prepare', sep='')))(that)
    class(that) <- 'SpotRateCurve'
    return(that)
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

#' @S3method as.data.frame SpotRateCurve
as.data.frame.SpotRateCurve <- function(curve, ...) {
    data.frame(terms=curve$terms, rates=curve$rates, list(...))
}

#' @S3method length SpotRateCurve
length.SpotRateCurve <- function(object) length(object$terms)

#' @S3method plot SpotRateCurve
'[.SpotRateCurve' <- function(object, term, forward.term=NULL, to.term=NULL) {
    if ( !is.null(to.term) || !is.null(forward.term) )
        return( forward.rate(object, term, forward.term, to.term) )
    SpotRate(object$interp.FUN(object, term), term, dib=object$dib)
}

#' @S3method plot SpotRateCurve
plot.SpotRateCurve <- function(curve, ...) {
    plot(curve$terms, curve$rates, ...)
}

#' Insert a SpotRate into the SpotRate.
#'
#' A SpotRateCurve can be expanded by inserting other SpotRate objects into it.
#'
#' @rdname insert
#' @export
'insert<-' <- function(object, ...) UseMethod('insert<-', object)

#' @rdname insert
#' @method insert<- SpotRateCurve
#' @S3method insert<- SpotRateCurve
#' @param object SpotRateCurve on which the SpotRate has to be inserted
#' @param value SpotRate to be inserted
#' @examples
#' # creating a SpotRateCurve from a matrix
#' mat <- cbind(c(0.08, 0.083, 0.089, 0.093, 0.095), c(0.5, 1, 1.5, 2, 2.5))
#' curve <- as.SpotRateCurve(mat, interp='Spline', dib=365)
#' insert(curve) <- SpotRate(0.085, 1.25, dib=365)
'insert<-.SpotRateCurve' <- function(object, value) {
    object$terms <- append(object$terms, value$term)
    object$rates <- append(object$rates, value$value)
    idx <- order(object$terms)
    object$terms <- object$terms[idx]
    object$rates <- object$rates[idx]
    object
}


