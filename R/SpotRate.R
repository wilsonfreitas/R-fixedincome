#' The spotrate object
#' 
#' @description
#' The \code{spotrate} represents a spot rate and stores the information needed
#' to compound or discount that rate.
#' 
#' @note
#' The \code{spotrate} class represents a annual spot rates. That
#' restriction might be removed in the future.
#' 
#' @details
#' 
#' The \code{spotrate} class fully specifies spot rates.
#' It has the 4 attributes necessary to manipulate spot rates:
#' \itemize{
#' \item the spot rate values which are numeric values representing the rate
#' \item the compounding regime that specifies how to compound the spot 
#' rate and is specified by a \code{compounding} object
#' \item the daycount rule which computes the compounding periods right 
#' adjusted to the spot rate frequency
#' \item the calendar that is responsible for computing days between 2 dates
#' }
#' 
#' The \code{spotrate} class is a numeric vector with attributes.
#' Once it is a numeric vector it represents spot rates sharing the same 
#' compounding, daycount and calendar attributes.
#' The coercion function \code{as.list.spotrate} splits the vector into many 
#' single spot rate objects.
#' 
#' @name spotrate-class
NULL


#' Crete a spotrate object
#' 
#' @description
#' A factory method of \code{spotrate} objects.
#' 
#' @note
#' The \code{spotrate} class represents a annual spot rates. That
#' restriction might be removed in the future.
#' 
#' @param obj a numeric value which defines a rate or a string specifying
#' a spot rate.
#' @param compounding the compounding regime can assume the following values:
#' \code{simple}, \code{compounded} and \code{continuous}
#' @param daycount daycount instance
#' @param calendar calendar instance
#' @param ... unused extra arguments
#' 
#' @return a spotrate object
#' @name as.spotrate
#' @examples
#' as.spotrate("0.06 simple actual/365")
#' library(bizdays)
#' as.spotrate(0.06, continuousCompounding(), as.daycount("actual/365"),
#' Calendar(name="actual"))
#' as.spotrate(c(0.06, 0.07, 0.08), simpleCompounding(),
#' as.daycount("actual/365"))
#' specs <- c("0.06 simple actual/365", "0.11 discrete business/252")
#' lapply(specs, as.spotrate)
NULL

#' @rdname as.spotrate
#' @export
as.spotrate <- function(obj, ...) UseMethod('as.spotrate', obj)

#' @rdname as.spotrate
#' @export
#' @details
#' \code{as.spotrate.default} accepts any objects that coerce to numeric.
#' The \code{compounding} and \code{daycount} arguments are necessary to
#' create a spotrate.
#' The \code{calendar} is only necessary if someone wants to compound 
#' spot rates providing dates (\code{from} and \code{to}) instead of terms.
#' 
as.spotrate.default <- function(obj, compounding, daycount, calendar=NULL, ...) {
	if (!is.numeric(obj))
	    stop('Invalid given rate:', obj)
	structure(obj, compounding=compounding, daycount=daycount, 
		calendar=calendar, class='spotrate')
}

#' @rdname as.spotrate
#' @export
#' @details
#' \code{as.spotrate.character} accepts a string in the format 
#' \code{RATE COMPOUNDING DAYCOUNT}, where:
#' \itemize{
#' \item \code{RATE} a numeric value
#' \item \code{COMPOUNDING} one of the following: \code{simple}, 
#' \code{discrete}, \code{continuous}
#' \item \code{DAYCOUNT} a valid day count rule, see 
#' \code{\link{daycount-class}}
#' }
#' That function returns a single spotrate (length 1).
#' If you have a character vector you should use the \code{lapply} function
#' to create a list of spotrates.
#' \preformatted{
#' specs <- c("0.06 simple actual/365", "0.11 discrete business/252")
#' lapply(specs, as.spotrate)
#' }
#' 
as.spotrate.character <- function(obj, ...) {
	if (length(obj) > 1)
		warning('length(obj) > 1. as.spotrate.character uses only the first.')
	lapply(strsplit(obj[1], '\\s+', perl=TRUE), function (x) {
		value <- as.numeric(x[1])
		compounding <- as.compounding(x[2])
		daycount <- as.daycount(x[3])
		as.spotrate(value, compounding, daycount)
	})[[1]]
}

#' @details
#' The \code{rates.spotrate} returns the numeric value representing the spot
#' rates.
#' 
#' @rdname rates
#' @export
rates.spotrate <- function(obj, ...) as.numeric(obj)

#' @rdname compounding
#' @export
compounding.spotrate <- function (obj, ...) attr(obj, 'compounding')

#' @rdname daycount-class
#' @export
daycount.spotrate <- function (obj, ...) attr(obj, 'daycount')

#' @rdname calendar
#' @export
calendar.spotrate <- function (obj, ...) attr(obj, 'calendar')

#' @rdname compound
#' @export
compound.spotrate <- function(obj, term, units=NULL, from=NULL, to=NULL, ...) {
	term <- if (missing(term)) {
		if (is.null(calendar(obj))) stop("Missing calendar")
		as.term(bizdays::bizdays(from, to, calendar(obj)), 'days')
	} else {
		tryCatch(as.term(term), error=function(e) {
			if (conditionMessage(e) == "Unknown units")
				as.term(term, if (is.null(units)) 'days' else units)
			else stop(e)
		})
	}
	comp <- compounding(obj)
	tf <- timefactor(daycount(obj), term=term)
	compound(comp, rates(obj), tf)
}

## standard generics

#' @export
as.list.spotrate <- function(x, ...) {
	dc <- daycount(x)
	com <- compounding(x)
	cal <- calendar(x)
	lapply(x, function(s) as.spotrate(s, com, dc, cal))
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
format.spotrate <- function (x, ...) {
	hdr <- sub(' +$', '', paste(compounding(x), daycount(x), 
		calendar(x)$name))
	paste(format(unclass(x), ...), hdr)
}

#' @export
as.character.spotrate <- function(x, ...) {
	if (length(x) == 1)
		sub(' +$', '', paste(rates(x), compounding(x), daycount(x), 
			calendar(x)$name))
	else {
		hdr <- sub(' +$', '', paste(compounding(x), daycount(x), 
			calendar(x)$name))
		paste(hdr, paste(rates(x), collapse=' '), sep='\n')
	}
}

#' @export
print.spotrate <- function(x, ...) {
	cat(as.character(x), '\n')
	invisible(x)
}

