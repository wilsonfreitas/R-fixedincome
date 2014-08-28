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

#' @export
as.spotratecurve.numeric <- function(obj, rates, units=c('days', 'months', 'years'), refdate=NULL, name=NULL, interp=linear) {
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
	if (! is.null(refdate)) {
		cal <- calendar(rates)
		if (is.null(cal))
			warning("rates doesn't have a calendar, refdate will be ignored")
		else
			obj <- as.Date(sapply(obj, function(x) add.bizdays(refdate, x, cal)), origin=as.Date('1970-01-01'))
	}
	class(rates) <- c('spotratecurve', 'spotrate')
	attr(rates, 'terms') <- obj
	attr(rates, 'units') <- match.arg(units)
	attr(rates, 'refdate') <- if (is.null(refdate)) refdate else as.Date(refdate)
	attr(rates, 'name') <- name
	attr(rates, 'interp') <- interp
	attr(rates, 'interp.handler') <- interp(rates)
	rates
}

#' @export
as.spotratecurve.Date <- function(obj, rates, refdate, name=NULL, interp=linear) {
	class(rates) <- c('spotratecurve', 'spotrate')
	attr(rates, 'terms') <- obj
	attr(rates, 'units') <- 'days'
	attr(rates, 'refdate') <- as.Date(refdate)
	attr(rates, 'name') <- name
	attr(rates, 'interp') <- interp
	attr(rates, 'interp.handler') <- interp(rates)
	rates
}

#' @export
as.spotratecurve.spotratecurve <- function(obj, refdate=NULL, name=NULL, interp=NULL) {
	if (! is.null(refdate))
		attr(obj, 'refdate') <- refdate
	if (! is.null(name))
		attr(obj, 'name') <- name
	if (! is.null(interp)) {
		attr(obj, 'interp') <- interp
		attr(obj, 'interp.handler') <- interp(obj)
	}
	obj
}

#' @export
terms.spotratecurve <- function(x, ..., units=NULL, as.x=FALSE) {
	.terms <- attr(x, 'terms')
	if (! is.null(units) || as.x) {
		.terms <- if (is(.terms, 'numeric')) .terms else
			bizdays(attr(x, 'refdate'), .terms, calendar(x))
		.terms <- as.term(.terms, attr(x, 'units'))
		if (! is.null(units))
			.terms <- as.term(daycount(x), .terms, units)
		.terms <- as.numeric(.terms)
	}
	.terms
}

#' @export
`[.spotratecurve` <- function(x, i) {
	if (any(i < 0))
		stop("spotratecurve does not handle negative subscripts.")
	if (! is(i, 'numeric'))
		i <- bizdays(attr(x, 'refdate'), i, calendar(x))
	sr <- attr(x, 'interp.handler')(i)
	as.spotrate(sr, compounding(x), daycount(x), calendar(x))
}

#' @export
`[<-.spotratecurve` <- function(x, i, value) {
	if (! is(i, 'numeric'))
		i <- bizdays(attr(x, 'refdate'), i, calendar(x))
	val <- cbind(i, value)
	i <- val[,1]
	value <- val[,2]
	terms <- terms(x, as.x=TRUE)
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
	sr <- as.spotrate(rates, compounding(x), daycount(x), calendar(x))
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

#' @export
print.spotratecurve <- function(x, ..., units=NULL) {
	m <- as.matrix(rates(x), ncol=1)
	.terms <- terms(x, units=units)
	if (is.numeric(.terms)) {
		.units <- if (is.null(units)) units(x) else units
		.terms <- as.term(.terms, units=.units)
	}
	rownames(m) <- format(.terms, digits=4)
	colnames(m) <- attr(x, 'name')
	print.default(m)
	cat(sub(' +$', '', paste(compounding(x), daycount(x), calendar(x)$name)), '\n')
	invisible(x)
}

#' @export
as.data.frame.spotratecurve <- function(x, ..., units=NULL, as.x=FALSE) {
	data.frame(terms=terms(x, units=units, as.x=as.x), rates=rates(x), ...)
}


#' @export
plot.spotratecurve <- function(curve, ...) {
	plot.default(x=terms(curve), y=rates(curve), xlab='Terms', ylab='Rates',
		main='Spot Rate Curve', ...)
}

#' @export
head.spotratecurve <- function(x, n=6L, ...) {
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


#' @export
units.spotratecurve <- function(x) attr(x, 'units')

#' @export
refdate <- function(x) attr(x, 'refdate')

#' @export
subcurve <- function(x, i) {
	.rates <- x[i]
	as.spotratecurve(i, .rates, name=attr(x, 'name'), interp=attr(x, 'interp'),
		refdate=refdate(x))
}

#' @export
interp <- function(x) UseMethod('interp', x)
`interp<-` <- function(x, value) UseMethod('interp<-', x)

#' @export
interp.spotratecurve <- function(x) attr(x, 'interp')

#' @export
`interp<-.spotratecurve` <- function(x, value) {
	attr(x, 'interp') <- value
	attr(x, 'interp.handler') <- value(x)
	x
}

#' @export
forwardrate <- function(obj, ...) UseMethod('forwardrate', obj)

# forwardrate.spotrate <- function(obj, from, rate.to, to,
# 								 units=c('days', 'months', 'years')) {
# 	units = match.arg(units)
# 	cf <- compound(rate.to, to)/compound(obj, from)
# 	comp <- compounding(obj)
# 	tf <- timefactor(daycount(obj), term=to-from, units=units(obj))
# 	as.spotrate(rates(comp, cf, tf), comp, daycount(obj), calendar(obj))
# }

#' @export
forwardrate.spotratecurve <- function(obj, from, to=NULL, forward=NULL) {
	# stopifnot(!is.null(to.term) || !is.null(forward.term))
	if ( ! is.null(to) ) {
		if (to <= from)
			stop("to term must be greater than from.")
		cf <- compound(obj[to], to)/compound(obj[from], from)
		comp <- compounding(obj)
		tf <- timefactor(daycount(obj), term=to-from, units=units(obj))
		as.spotrate(rates(comp, cf, tf), comp, daycount(obj), calendar(obj))
	} else {
		if (forward == 1 && from == 1)
			obj[1]
		else {
			to <- from+forward
			cf <- compound(obj[to], to)/compound(obj[from], from)
			comp <- compounding(obj)
			tf <- timefactor(daycount(obj), term=to-from, units=units(obj))
			as.spotrate(rates(comp, cf, tf), comp, daycount(obj), calendar(obj))
		}
	}
}
