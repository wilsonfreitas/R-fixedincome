#' Day count convention class
#' 
#' @description
#' Day count convetion creation.
#' 
#' @details
#' Day count convetion rules are used to compute terms used to discount and
#' compound interest rates.
#' 
#' @rdname daycount
#' @examples
#' dc <- as.daycount('actual/360')
#' dib(dc)
#' timefactor(dc, 10, 'days')
#' as.term(dc, '10 days', 'months')

#' @export
daycount <- function(object, ...) UseMethod('daycount', object)

.daycounts.dib <- list(
	'30/360' = 360,
	'30E/360' = 360,
	'actual/365' = 365,
	'actual/360' = 360,
	'business/252' = 252
)

#' @rdname daycount
#' @export
as.daycount <- function(daycount) {
	if ( !any(daycount == names(.daycounts.dib)) )
		stop('Unknown daycount: ', daycount)
	dc_parts <- unlist(strsplit(daycount, '/'))
	attr(daycount, "dib") <- as.numeric(dc_parts[2])
	class(daycount) <- "daycount"
	daycount
}

#' @export
as.character.daycount <- function(daycount) {
	attributes(daycount) <- NULL
	as.character(daycount)
}

#' @export
print.daycount <- function(daycount, ...) cat(daycount, '\n')

#' @export
dib.daycount <- function(daycount) attr(daycount, 'dib')

#' @export
timefactor <- function(object, ...) UseMethod('timefactor', object)

#' @export
timefactor.daycount <- function(daycount, term, units=NULL) {
	tm <- as.term(term, units)
	if (units(tm) == 'days') as.numeric(tm)/dib(daycount)
	else as.numeric(as.term(tm, units='years'))
}

#' @export
as.term.daycount <- function(daycount, term, units) {
	tm <- as.term(term)
	if (units(tm) == 'days') {
		tm <- switch(units, years=as.numeric(tm)/dib(daycount),
			months=as.numeric(tm)/(dib(daycount)/12),
			days=as.numeric(tm))
	} else if (units == 'days') {
		tm <- switch(units(tm), years=as.numeric(tm)*dib(daycount),
			months=as.numeric(tm)*(dib(daycount)/12),
			days=as.numeric(tm))
	}
	as.term(tm, units)
}


