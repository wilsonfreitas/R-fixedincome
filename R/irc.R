
irc <- function(terms, rates, calendar=NULL, compound=NULL, refdate=Sys.Date(),
	name=NULL) {
	# rates and terms must have the same length
	# stopifnot(length(rates) == length(terms))
	# terms must not have duplicated elements
	stopifnot(length(terms) == length(unique(terms)))
	# terms must be crescent
	stopifnot(all(diff(terms) > 0))
	# creating irc -- extending matrix
	rates <- matrix(rates, nrow=length(terms), ncol=1)
	rownames(rates) <- terms
	# attributes
	if ( is(refdate, c('POSIXct', 'POSIXlt', 'character')) )
		refdate <- as.Date(refdate)
	attr(rates, 'refdate') <- refdate
	attr(rates, 'compound') <- compound
	attr(rates, 'name') <- name
	# returning class
	class(rates) <- 'irc'
	rates
}

