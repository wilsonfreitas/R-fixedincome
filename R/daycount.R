
#' Spot rate day count convention
#' 
#' @export daycount
#' @rdname daycount
daycount <- function(object, ...) UseMethod('daycount', object)

.daycounts.dib <- list(
	'30/360' = 360,
	'30E/360' = 360,
	'actual/365' = 365,
	'actual/360' = 360,
	'business/252' = 252
)

as.daycount <- function(daycount, calendar=NULL) {
	if ( !any(daycount == names(.daycounts.dib)) )
		stop('Unknown daycount: ', daycount)
	dc_parts <- unlist(strsplit(daycount, '/'))
	attr(daycount, "dib") <- as.numeric(dc_parts[2])
	class(daycount) <- "daycount"
	daycount
}

as.character.daycount <- function(daycount) {
	attributes(daycount) <- NULL
	as.character(daycount)
}

print.daycount <- function(daycount, ...) cat(daycount, '\n')

dib.daycount <- function(daycount) attr(daycount, 'dib')

timefactor <- function(object, ...) UseMethod('timefactor', object)

timefactor.daycount <- function(daycount, term, units=NULL) {
	tm <- as.term(term, units)
	if (units(tm) == 'days') as.numeric(tm)/dib(daycount)
	else as.numeric(as.term(tm, units='years'))
}

