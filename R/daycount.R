#' Day count convention class
#' 
#' @description
#' \code{daycount} class computes terms used to discount and
#' compound interest rates.
#' 
#' @details
#' The day count convention determines the amount of days, in years, for a
#' given term.
#' 
#' \code{as.daycount} creates a \code{daycount} object and accepts the
#' following options: \code{30/360}, \code{30E/360}, \code{actual/365},
#' \code{actual/360}, \code{business/252}
#' 
#' \code{dib} returns the days in base for a day count convention. Since we 
#' work with annual rates the days in base define the amount of days in a year.
#' Given that rule the amount of days in a month is obtained by the fraction of
#' 1/12 of days in base \code{dib}.
#' 
#' \code{timefactor} returns the given term in years, since we are assuming we 
#' work with annual rates. The \code{term} argument can be a term instance, 
#' a string defining a term or a number, but in the last a \code{units} must be
#' provided.
#' 
#' \code{as.term} is a convertion function, it converts terms units. Since we
#' don't have the amount of days per year, while creating a term we can't make
#' conversions involving days. That's the reason why that function exists, to 
#' convert using days. The \code{term} argument must be a valid term or a 
#' valid string defining a term. The argument \code{units} is the resulting 
#' units.
#' 
#' @param obj an instance of \code{daycount}
#' @param dcspec a string defining the day count convention: \code{30/360},
#' \code{30E/360}, \code{actual/365}, \code{actual/360}, \code{business/252}
#' @param term a valid term
#' @param units a valid units (\code{days}, \code{months}, \code{years})
#' @param ... extra arguments
#' 
#' @name daycount-class
#' @examples
#' dc <- as.daycount('actual/360')
#' dib(dc)
#' timefactor(dc, 10, 'days')
#' as.term(dc, '10 days', 'months')
NULL

.daycounts.dib <- list(
	'30/360' = 360,
	'30E/360' = 360,
	'actual/365' = 365,
	'actual/360' = 360,
	'business/252' = 252
)

#' @export
as.daycount <- function(x, ...) UseMethod('as.daycount', x)

#' @rdname daycount-class
#' @export
as.daycount.character <- function(x, ...) {
	dcspec <- x
	if ( !any(dcspec == names(.daycounts.dib)) && !grepl('fixed/', dcspec))
		stop('Unknown daycount: ', dcspec)
	dc_parts <- unlist(strsplit(dcspec, '/'))
	attr(dcspec, "dib") <- as.numeric(dc_parts[2])
	class(dcspec) <- "daycount"
	dcspec
}

#' @export
as.daycount.daycount <- identity

#' @export
as.character.daycount <- function(x, ...) {
	attributes(x) <- NULL
	as.character(x)
}

#' @export
print.daycount <- function(x, ...) cat(x, '\n')

#' @rdname daycount-class
#' @export
dib <- function(obj, ...) UseMethod('dib', obj)

#' @rdname daycount-class
#' @export
dib.daycount <- function(obj, ...) attr(obj, 'dib')

#' @rdname daycount-class
#' @export
timefactor <- function(obj, ...) UseMethod('timefactor', obj)

#' @rdname daycount-class
#' @export
timefactor.daycount <- function(obj, term, units=NULL, ...) {
	tm <- as.term(term, units)
	if (units(tm) == 'days') as.numeric(tm)/dib(obj)
	else as.numeric(as.term(tm, units='years'))
}

#' @rdname daycount-class
#' @export
as.term.daycount <- function(obj, term, units, ...) {
	tm <- as.term(term)
	if (units(tm) == 'days') {
		tm <- switch(units, years=as.numeric(tm)/dib(obj),
			months=as.numeric(tm)/(dib(obj)/12),
			days=as.numeric(tm))
	} else if (units == 'days') {
		tm <- switch(units(tm), years=as.numeric(tm)*dib(obj),
			months=as.numeric(tm)*(dib(obj)/12),
			days=as.numeric(tm))
	}
	as.term(tm, units)
}


#' Day count accessor
#' 
#' @description
#' Method to access the \code{daycount} attribute.
#' 
#' @param obj any object which has a daycount attribute
#' @param ... extra arguments
#' 
#' @return a \code{daycount} object
#' 
#' @name daycount
NULL

#' @rdname daycount
#' @export
daycount <- function(obj, ...) UseMethod('daycount', obj)
