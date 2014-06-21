#' Create a spotrate spr.
#' 
#' Creates a spot rate that is an interest rate related to a specific term.
#' It can be interpreted as the interest amount asked to for investments 
#' maturing at the term.
#' 
#' A spotrate is composed by the value of its interest rate, the term, the 
#' amount of days in basis (days within a year) and the compounding regime
#' on which the rate is compounded.
#' The spotrate can be compounded to generate a CompoundFactor, and that 
#' can be used in many calculations, for example, computing equivalent rates.
#' 
#' @param value the value of the underlying interest rate
#' @param term the term related to the interest paid
#' @param dib days in base (the number of days within a year)
#' @param compounding the compounding regime can assume the following values:
#' \code{simple}, \code{compounded} and \code{continuous}
#' @return a spotrate spr
#' @name spotrate-class
NULL

#' @export
spotrate <- function(value, compounding, daycount, calendar=NULL) {
	if (!is.numeric(value))
	    stop('Invalid given rate:', value)
	structure(value, compounding=compounding, daycount=daycount, 
		calendar=calendar, class='spotrate')
}

#' @export
as.character.spotrate <- function(spr) {
	if (length(spr) == 1)
		sub(' +$', '', paste(rates(spr), compounding(spr), daycount(spr), 
			calendar(spr)$name))
	else {
		hdr <- sub(' +$', '', paste(compounding(spr), daycount(spr), 
			calendar(spr)$name))
		paste(hdr, paste(rates(spr), collapse=' '), sep='\n')
	}
}

#' @export
print.spotrate <- function(x, ...) cat(as.character(x), '\n')

#' @export
is.spotrate <- function(object) class(object) == 'spotrate'

#' @export
as.spotrate <- function(object, ...) UseMethod('as.spotrate', object)

#' @export
rates.spotrate <- function(spr) as.numeric(spr)

#' @export
frequency.spotrate <- function(spr) attr(spr, 'frequency')

#' @export
compounding.spotrate <- function (spr) attr(spr, 'compounding')

#' @export
daycount.spotrate <- function (spr) attr(spr, 'daycount')

#' @export
calendar.spotrate <- function (spr) attr(spr, 'calendar')

#' @export
compound.spotrate <- function(spr, term, units=NULL, from=NULL, to=NULL) {
	term <- if (missing(term)) {
		if (is.null(calendar(spr))) stop("Missing calendar")
		as.term(bizdays(from, to, calendar(spr)), 'days')
	} else {
		tryCatch(as.term(term), error=function(e) {
			if (conditionMessage(e) == "Unknown units")
				as.term(term, if (is.null(units)) 'days' else units)
			else stop(e)
		})
	}
	comp <- compounding(spr)
	tf <- timefactor(daycount(spr), term=term)
	compound(comp, rates(spr), tf)
}

#' @export
as.data.frame.spotrate <- function (x, row.names=NULL, optional=FALSE, ..., nm=paste(deparse(substitute(x), width.cutoff=500L), collapse=" ")) {
	force(nm)
	nrows <- length(x)
	if (is.null(row.names)) {
		if (nrows == 0L) 
			row.names <- character()
		else if (length(row.names <- names(x)) == nrows && !anyDuplicated(row.names)) {
		}
		else row.names <- .set_row_names(nrows)
	}
	if (!is.null(names(x))) 
		names(x) <- NULL
	value <- list(x)
	if (!optional) 
		names(value) <- nm
	attr(value, "row.names") <- row.names
	class(value) <- "data.frame"
	value
}

#' @export
`[.spotrate` <- function (x, ..., drop = TRUE) {
	cl <- oldClass(x)
	class(x) <- NULL
	val <- NextMethod("[")
	class(val) <- cl
	attr(val, "compounding") <- attr(x, "compounding")
	attr(val, "daycount") <- attr(x, "daycount")
	attr(val, "calendar") <- attr(x, "calendar")
	val
}

#' @export
format.spotrate <- function (x, ...) as.character(x)

