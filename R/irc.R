
#' @export
irc <- function(terms, rates, calendar=NULL, compound=NULL,
	interpolation=linear, refdate=Sys.Date(),
	name=NULL) {
	# rates and terms must have the same length
	# stopifnot(length(rates) == length(terms))
	# terms must not have duplicated elements
	stopifnot(length(terms) == length(unique(terms)))
	# terms must be crescent
	stopifnot(all(diff(terms) > 0))
	# creating irc -- extending matrix
	that <- list()
	that$data <- matrix(rates, nrow=length(terms), ncol=1)
	if ( any(c('POSIXct', 'POSIXlt', 'character') %in% class(terms)) )
		terms <- as.Date(terms)
	rownames(that$data) <- terms
	# attributes
	if ( is(refdate, c('POSIXct', 'POSIXlt', 'character')) )
		refdate <- as.Date(refdate)
	that$refdate <- refdate
	that$compound <- compound
	that$interpolation <- interpolation
	that$name <- name
	# returning class
	class(that) <- 'irc'
	that
}

#' @S3method [ irc
'[.irc' <- function(obj, terms) {
	as.numeric(obj$data[as.character(terms), 1])
}