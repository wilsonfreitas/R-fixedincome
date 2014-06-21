#' Generic term class
#' 
#' @description
#' Terms creation, printing, and some arithmetic.
#' 
#' @details
#' Terms are time intervals in days, months or years, which are appropriate
#' for fixed income calculations.
#' The term class represents the period used to discount or compound a spot 
#' rate.
#' That period represented by the term can be in days, years or months and
#' that is the term units.
#' 
#' Conversions are allowed from months to years and vice versa.
#' Conversions involving days cannot be done since the number of days for 
#' months and years varies from one day count convetion to another.
#' 
#' The term class inherits from the difftime class (base package).
#' 
#' @param obj can be another \code{term} instance, a string specifing a 
#' \code{term} or a number, and if the last a \code{units} must be provided.
#' @param units one of the valid \code{units}: \code{days}, \code{monts}, 
#' \code{years}.
#' @param ... extra arguments
#' 
#' @name term-class
#' @examples
#' as.term(6, 'months')
#' as.term('6 months')
#' as.term(as.term('6 months'), units='years')
#' as.numeric(as.term('6 months'))
NULL

#' @rdname term-class
#' @export
as.term <- function(obj, ...) UseMethod('as.term', obj)

#' @rdname term-class
#' @export
as.term.numeric <- function(obj, units, ...) {
	if (missing(units)) stop('Unknown units')
	if (!any(units == c('years', 'months', 'days')))
		stop('Unknown units: ', units)
	attr(obj, 'units') <- units
	class(obj) <- c('term', 'difftime')
	obj
}

#' @rdname term-class
#' @export
as.term.character <- function(obj, units=NULL, ...) {
	m <- regexec('^([0-9]+)(\\.[0-9]+)? (years|months|days)?$', obj)
	m <- unlist(regmatches(obj, m))
	if (length(m))
		t <- as.term(as.numeric(paste0(m[2], m[3])), m[4])
	else
		stop("Invalid term: ", obj)
	if (is.null(units)) t
	else as.term(t, units)
}

#' @rdname term-class
#' @export
as.term.term <- function(obj, units=NULL, ...) {
	if (is.null(units))
		return(obj)
	if (!any(units == c('years', 'months')))
		stop('Cannot convert to given units: ', units)
	r <- list(months=list(months=1, years=12), years=list(months=1/12, years=1))
	as.term(as.numeric(obj)*r[[units]][[units(obj)]], units)
}

#' @export
as.character.term <- function(x, ...) {
	if (length(x) == 1)
		paste(unclass(x), units(x))
	else {
		hdr <- paste('terms in', units(x))
		paste(hdr, paste(unclass(x), collapse=' '), sep='\n')
	}
}

#' @export
print.term <- function(x, ...) cat(as.character(x), '\n')

