
#' term
#' 
#' Given term
#' 
#' @export
term <- function(object, ...) UseMethod('term', object)

as.term <- function(object, ...) UseMethod('as.term', object)

as.term.numeric <- function(term, units) {
	if (missing(units)) stop('Unknown units')
	if (!any(units == c('years', 'months', 'days')))
		stop('Unknown units: ', units)
	attr(term, 'units') <- units
	class(term) <- c('term', 'difftime')
	term
}

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

as.term.term <- function(term, units=NULL) {
	if (is.null(units))
		return(term)
	if (!any(units == c('years', 'months')))
		stop('Cannot convert to given units: ', units)
	r <- list(months=list(months=1, years=12), years=list(months=1/12, years=1))
	as.term(as.numeric(term)*r[[units]][[units(term)]], units)
}

units.term <- function(term) attr(term, 'units')

as.character.term <- function(term) {
	if (length(term) == 1)
		paste(unclass(term), units(term))
	else {
		hdr <- paste('terms in', units(term))
		paste(hdr, paste(unclass(term), collapse=' '), sep='\n')
	}
}

print.term <- function(term, ...) cat(as.character(term), '\n')

