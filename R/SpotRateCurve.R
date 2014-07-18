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
	class(rates) <- c('spotratecurve', 'spotrate')
	attr(rates, 'interp') <- interp
	attr(rates, 'interp.handler') <- if (is.null(interp)) interp else interp(rates)
	rates
}

#' @export
terms.spotratecurve <- function(x, ...) attr(x, 'terms')

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

# my <- function(x) UseMethod('my')
# my.c1 <- function(x) {cat('c1\n'); NextMethod(); x}
# my.c2 <- function(x) {cat('c2\n'); NextMethod(); x}
# my.c3 <- function(x) {cat('c3\n'); x}
# my.default <- function(x) {cat('default\n'); x}
# x <- 1; class(x) <- c('c1', 'c2'); my(x)

#' @export
`[.spotratecurve` <- function(x, terms) {
	sr <- attr(x, 'interp.handler')(terms)
	as.spotrate(sr, compounding=compounding(x), daycount=daycount(x),
		calendar(x))
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
`[<-.spotratecurve` <- function(x, i, value) {
	val <- cbind(i, value)
	i <- val[,1]
	value <- val[,2]
	terms <- terms(x)
	rates <- rates(x)
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
	sr <- as.spotrate(rates, compounding=compounding(x), daycount=daycount(x),
		calendar(x))
	as.spotratecurve(terms, sr, name=attr(x, 'name'), interp=attr(x, 'interp'),
		refdate=attr(x, 'refdate'))
}

#' spotratecurve generic extensions
#' 
#' spotratecurve generic extensions
#' 
#' @rdname generic-spotratecurve
#' @name generic-spotratecurve
NULL

print.spotratecurve <- function(x, ...) {
	m <- as.matrix(rates(x), ncol=1)
	rownames(m) <- terms(x)
	colnames(m) <- attr(x, 'name')
	print.default(m)
	invisible(x)
}

# as.data.frame.spotratecurve <- function(curve, ...) {
#     data.frame(terms=terms(curve), rates=rates(curve), ...)
# }

# length.spotratecurve <- function(curve) dim(curve)[1]

# plot.spotratecurve <- function(curve, ...) {
#     plot(terms(curve), rates(curve), ...)
# }

#' @export
head.spotratecurve <- function(x, n=6L, ...) {
	# stopifnot(length(curve) >= n)
	# curve[terms(curve)[seq_len(n)]]
	stopifnot(length(n) == 1L)
	n <- if (n < 0L) max(length(x) + n, 0L) else min(n, length(x))
	idx <- terms(x)[seq_len(n)]
	as.spotratecurve(idx, x[idx], name=attr(x, 'name'),
		interp=attr(x, 'interp'),
		refdate=attr(x, 'refdate'))
}

#' @export
tail.spotratecurve <- function(x, n=6L, ...) {
	stopifnot(length(n) == 1L)
	xlen <- length(x)
	n <- if (n < 0L) max(xlen + n, 0L) else min(n, xlen)
	idx <- terms(x)[seq.int(to = xlen, length.out = n)]
	as.spotratecurve(idx, x[idx], name=attr(x, 'name'),
		interp=attr(x, 'interp'),
		refdate=attr(x, 'refdate'))
}

