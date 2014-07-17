#' Creates a spotratecurve
#' 
#' spotratecurve abstracts a term structure class providing methods to operate 
#' on the term structure internals.
#' 
#' Actually, the class spotratecurve represents a zero curve.
#' A zero curve is a set of \code{rates} indexed by \code{terms}.
#' Each pair (rate,term) is a spot rate (\code{\link{SpotRate}}) and
#' it defines the cost of a loan maturing in the term.
#' Once a spotratecurve is created, any rate can be retrieved by its term 
#' through the index operator (\code{`[`}) which returns a
#' \code{\link{SpotRate}}.
#' For those terms which don't have a corresponding rate, the interpolation 
#' method is used to generate a rate.
#' The parameter \code{interp} specifies the interpolation method to use.
#' Since the operator \code{`[`} returns a SpotRate the \code{dib} must be
#' provided to generate spot rates accordingly.
#'
#' @name spotratecurve-class
#' @param rates a vector with the interest rates
#' @param terms a vector with the terms
#' @param interp defines which interpolation method to use: 
#' Linear, FlatForward, LogLinear, Hermite Cubic Spline, Monotone Cubic Spline,
#' Natural Cubi Spline
#' @return spotratecurve object.
#' @export
#' @examples
#' # Creating a zero curve 
#' curve <- spotratecurve(c(0.08, 0.083, 0.089, 0.093, 0.095), 
#' c(0.5, 1, 1.5, 2, 2.5))
#' # Creating a zero curve with linear interpolation and 360 days in base 
#' # (days per year)
#' curve <- spotratecurve(c(0.08, 0.083, 0.089, 0.093, 0.095), 
#' c(0.5, 1, 1.5, 2, 2.5), dib=360, compounding='simple')
NULL


#' Coerce objects to spotratecurve
#' 
#' A spotratecurve can be coerced from \code{matrix} and \code{data.frame}.
#'
#' @name as.spotratecurve
#' @param object should be a \code{matrix} or a \code{data.frame}
#' @param ... other spotratecurve's parameters
#' @return spotratecurve object
#' @rdname as.spotratecurve
#' @export as.spotratecurve
NULL

#' @rdname as.spotratecurve
#' @export
as.spotratecurve <- function(obj, ...) UseMethod('as.spotratecurve', obj)

as.spotratecurve.numeric <- function(obj, rates, refdate=NULL, interp=NULL, name=NULL) {
	if (length(obj) != length(rates))
		stop("rates and terms must have the same length.")
	if (length(obj) != length(unique(obj)))
		stop("terms cannot have duplicated elements.")
	if (! all(obj > 0))
		stop("all terms must be strictly positive.")
	if (! all(diff(obj) > 0))
		stop("terms must be ordered ascending.")
	if (! is(rates, 'spotrate'))
		stop("rates must be an instance of spotrate.")
	# dim(rates) <- c(length(rates), 1)
	# rownames(rates) <- terms
	attr(rates, 'terms') <- obj
	attr(rates, 'refdate') <- if (is.null(refdate)) refdate else as.Date(refdate)
	attr(rates, 'name') <- name
	attr(rates, 'interp') <- name
	class(rates) <- c('spotratecurve', 'spotrate')
	rates
}

#' @export
terms.spotratecurve <- function(obj) attr(obj, 'terms')

#' @return spotratecurve object
#' 
#' @rdname as.spotratecurve
#' @method as.spotratecurve data.frame
#' @S3method as.spotratecurve data.frame
#' @examples
#' # creating a spotratecurve from a data.frame
#' df <- data.frame(rates=c(0.08, 0.083, 0.089, 0.093, 0.095), terms=c(0.5, 1, 1.5, 2, 2.5))
#' curve <- as.spotratecurve(df, interp='Linear', dib=360)
as.spotratecurve.data.frame <- function(object, ...) {
    spotratecurve(rates=object[,1], terms=object[,2], ...)
}

#' @return spotratecurve object
#' 
#' @rdname as.spotratecurve
#' @method as.spotratecurve data.frame
#' @S3method as.spotratecurve data.frame
#' @examples
#' # creating a spotratecurve from a matrix
#' mat <- cbind(c(0.08, 0.083, 0.089, 0.093, 0.095), c(0.5, 1, 1.5, 2, 2.5))
#' curve <- as.spotratecurve(mat, interp='Spline', dib=365)
as.spotratecurve.matrix <- function(object, ...) {
    spotratecurve(rates=object[,1], terms=object[,2], ...)
}

#' @rdname open-brace.spotratecurve
#' @export
'[.spotratecurve' <- function(object, term) {
    if (any(term > 0)) {
        spotratecurve(rates(object)[terms(object) %in% term],
            term, dib=dib(object), compounding=compounding(object))
    } else {
        term <- abs(term)
        idx <- which(terms(object) %in% term)
        spotratecurve(rates(object)[-idx],
            terms(object)[-idx],
            dib=dib(object), compounding=compounding(object))
    }
}

#' Get the SpotRate for a given term
#' 
#' Get the SpotRate for a given term
#' 
#' @export
getSpotRate <- function(curve, term) {
    stopifnot(length(term) == 1)
    SpotRate(curve[term], term, dib=dib(curve), compounding=compounding(curve))
}

#' @rdname getSpotRate
#' @method [[ spotratecurve
#' @S3method [[ spotratecurve
'[[.spotratecurve' <- function(curve, term) {
    getSpotRate(curve, term)
}

#' Insert a SpotRate into the SpotRate.
#'
#' A spotratecurve can be expanded by inserting other SpotRate objects into it.
#'
#' @rdname open-brace.spotratecurve
#' @method [<- spotratecurve
#' @S3method [<- spotratecurve
#' @param object spotratecurve on which the SpotRate has to be inserted
#' @param value SpotRate to be inserted
#' @examples
#' # creating a spotratecurve from a matrix
#' mat <- cbind(c(0.08, 0.083, 0.089, 0.093, 0.095), c(0.5, 1, 1.5, 2, 2.5))
#' curve <- as.spotratecurve(mat, interp='Spline', dib=365)
#' curve[1.25] <- 0.085
'[<-.spotratecurve' <- function(object, i, value) {
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
    spotratecurve(rates, terms, dib(object), compounding(object))
}

#' spotratecurve generic extensions
#' 
#' spotratecurve generic extensions
#' 
#' @rdname generic-spotratecurve
#' @name generic-spotratecurve
NULL

# print.spotratecurve <- function(x, ...) {
#     m <- as.matrix(rates(x), ncol=1)
#     rownames(m) <- terms(x)
#     colnames(m) <- attr(x, 'name')
#     print(m)
# }

# as.data.frame.spotratecurve <- function(curve, ...) {
#     data.frame(terms=terms(curve), rates=rates(curve), ...)
# }

# length.spotratecurve <- function(curve) dim(curve)[1]

# plot.spotratecurve <- function(curve, ...) {
#     plot(terms(curve), rates(curve), ...)
# }

# head.spotratecurve <- function(curve, n=6L, ...) {
# 	stopifnot(length(curve) >= n)
# 	curve[terms(curve)[seq_len(n)]]
# }

# tail.spotratecurve <- function(curve, n=6L, ...) {
# 	stopifnot(length(curve) >= n)
# 	.terms <- tail(seq_along(terms(curve)), n)
# 	.terms <- terms(curve)[.terms]
# 	curve[.terms]
# }

# rates.spotratecurve <- function(object) as.numeric(object)

# rates.CurveInterpolation <- function(object, terms=NULL) {
# 	if (is.null(terms))
# 		NextMethod('rates', object)
# 	else {
# 		interp <- attr(object, 'interp')
# 		interp(terms)
# 	}
# }

