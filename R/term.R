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
#' @param term can be another \code{term} instance, a string specifing a 
#' \code{term} or a number, and if the last a \code{units} must be provided.
#' @param units one of the valid \code{units}: \code{days}, \code{monts}, 
#' \code{years}.
#' 
#' @name term-class
#' @examples
#' as.term(6, 'months')
#' as.term('6 months')
#' as.term(as.term('6 months'), units='years')
#' as.numeric(as.term('6 months'))
NULL

#' @export
as.term <- function(object, ...) UseMethod('as.term', object)

#' @rdname term-class
#' @export
as.term.numeric <- function(term, units) {
	if (missing(units)) stop('Unknown units')
	if (!any(units == c('years', 'months', 'days')))
		stop('Unknown units: ', units)
	attr(term, 'units') <- units
	class(term) <- c('term', 'difftime')
	term
}

#' @rdname term-class
#' @export
as.term.character <- function(term, units=NULL) {
	m <- regexec('^([0-9]+)(\\.[0-9]+)? (years|months|days)?$', term)
	m <- unlist(regmatches(term, m))
	if (length(m))
		t <- as.term(as.numeric(paste0(m[2], m[3])), m[4])
	else
		stop("Invalid term: ", term)
	if (is.null(units)) t
	else as.term(t, units)
}

#' @rdname term-class
#' @export
as.term.term <- function(term, units=NULL) {
	if (is.null(units))
		return(term)
	if (!any(units == c('years', 'months')))
		stop('Cannot convert to given units: ', units)
	r <- list(months=list(months=1, years=12), years=list(months=1/12, years=1))
	as.term(as.numeric(term)*r[[units]][[units(term)]], units)
}

#' @export
as.character.term <- function(term) {
	if (length(term) == 1)
		paste(unclass(term), units(term))
	else {
		hdr <- paste('terms in', units(term))
		paste(hdr, paste(unclass(term), collapse=' '), sep='\n')
	}
}

#' @export
print.term <- function(term, ...) cat(as.character(term), '\n')

