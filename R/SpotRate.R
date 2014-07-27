#' The spotrate object
#' 
#' @description
#' The \code{spotrate} object abstracts a spot rate (or an interst rate) and
#' stores all information needed to compound or discount that rate.
#' 
#' @note
#' The \code{spotrate} objects are annual spot rates. 
#' That restriction might be removed in the future.
#' 
#' @details
#' The \code{spotrate} class fully specifies spot rates.
#' It has:
#' \itemize{
#' \item the spot rate values which are numeric values representing the rate.
#' \item the compounding regime that specifies how to compound the spot 
#' rate. This is a \code{compounding} object.
#' \item the daycount rule to compute the compounding periods right 
#' adjusted to the spot rate frequency (which is annual).
#' \item the calendar which returns the number of days between 2 dates.
#' }
#' 
#' The \code{spotrate} class is a \code{numeric} vector and 
#' all values in a \code{spotrate} instance share the 
#' \code{compounding}, \code{daycount} and \code{calendar} attributes.
#' The coercion function \code{as.list.spotrate} splits the vector into many 
#' single spot rate objects.
#' 
#' The \code{calendar} attribute is an instance of \code{bizdays}
#' \code{\link[bizdays]{Calendar}} class.
#' 
#' @name spotrate-class
#' @examples
#' as.spotrate("0.06 simple actual/365")
#' library(bizdays)
#' as.spotrate(0.06, as.compounding('continuous'), as.daycount("actual/365"),
#' Calendar(name="actual"))
#' as.spotrate(c(0.06, 0.07, 0.08), as.compounding('simple'),
#' as.daycount("actual/365"))
#' specs <- c("0.06 simple actual/365", "0.11 discrete business/252")
#' lapply(specs, as.spotrate)
NULL

#' Create a spotrate object
#' 
#' @description
#' A factory method of \code{spotrate} objects.
#' 
#' @param obj numeric values defining rates or a string specifying
#' a spot rate
#' @param compounding an instance of \code{\link{compounding-class}}
#' @param daycount an instance of \code{\link{daycount-class}}
#' @param calendar and instance of bizdays' \code{\link[bizdays]{Calendar}}
#' @param ... extra arguments
#' 
#' @return a spotrate object
#' 
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
as.spotrate.default <- function(obj, compounding, daycount, calendar=NULL, ...) {
	as.spotrate(as.numeric(obj), compounding, daycount, calendar)
}

#' @rdname as.spotrate
#' @export
#' @details
#' The \code{compounding} and \code{daycount} arguments are necessary to
#' create a spotrate.
#' The \code{calendar} argument is only necessary if someone wants to compound 
#' spot rates between dates.
#' 
as.spotrate.numeric <- function(obj, compounding, daycount, calendar=NULL, ...) {
	structure(obj, compounding=as.compounding(compounding),
		daycount=as.daycount(daycount), 
		calendar=calendar, class='spotrate')
}

#' @rdname as.spotrate
#' @export
#' @details
#' \code{as.spotrate.character} accepts a string in the format 
#' 
#' \preformatted{"RATE COMPOUNDING DAYCOUNT"}
#' 
#' where:
#' \itemize{
#' \item \code{RATE} is a numeric value
#' \item \code{COMPOUNDING} is one of the following strings \code{simple}, 
#' \code{discrete}, \code{continuous}
#' \item \code{DAYCOUNT} is a valid day count rule, see 
#' \code{\link{daycount-class}}
#' }
#' 
#' That function returns a single \code{spotrate} (length 1).
#' If you have a character vector you should use the \code{lapply} function
#' to create a list of \code{spotrate}.
#' 
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

#' @rdname as.spotrate
#' @export
as.spotrate.spotrate <- function(obj, term, compounding=NULL, daycount=NULL, calendar=NULL, ...) {
	compounding <- if (is.null(compounding)) compounding(obj) else compounding
	daycount <- if (is.null(daycount)) daycount(obj) else daycount
	calendar <- if (is.null(calendar)) calendar(obj) else calendar
	f <- compound(obj, term)
	fun <- function(r) {
		sr <- as.spotrate(r, compounding, daycount, calendar)
		f - compound(sr, term)
	}
	eq_rate <- .imprate(fun, c(1e-10, 10), obj)
	as.spotrate(eq_rate, compounding=compounding, daycount=daycount, 
		calendar=calendar)
}

.imprate <- function(f, rng, ini, eps=.Machine$double.eps, max.iter=1000) {
	r.up <- rng[2]*rep(1, length(ini))
	r.down <- rng[1]*rep(1, length(ini))
	r <- as.numeric(ini)
	iter <- 0
	err <- f(r)
	## repeat until error is sufficiently small or counter hits 1000
	while (all(abs(err) > eps) && iter < max.iter) {
		idx.err <- err > 0
		r.down[idx.err] <- r[idx.err]
		r[idx.err] <- (r.up[idx.err] + r[idx.err])/2
		idx.err <- !idx.err
		r.up[idx.err] <- r[idx.err]
		r[idx.err] <- (r.down[idx.err] + r[idx.err])/2
		err <- f(r)
		iter <- iter + 1
	}
	r
}


#' @details
#' If the \code{obj} argument is a \code{spotrate} instance it 
#' returns a \code{numeric} representing the spot rates.
#' 
#' @rdname rates
#' @export
rates.spotrate <- function(obj, ...) as.numeric(obj)

#' @rdname compounding
#' @export
compounding.spotrate <- function (obj, ...) attr(obj, 'compounding')

#' @rdname daycount
#' @export
daycount.spotrate <- function (obj, ...) attr(obj, 'daycount')

#' @rdname calendar
#' @export
calendar.spotrate <- function (obj, ...) attr(obj, 'calendar')

#' @details
#' If \code{obj} is a \code{\link{spotrate-class}} the \code{term} can be a 
#' \code{\link{term-class}} or a numeric.
#' For numeric term the \code{units} argument should be used or it defaults to
#' \code{"days"}.
#' When the \code{term} argument is missing the arguments \code{from} and 
#' \code{to} must be provided and also the spot rate's \code{calendar}  
#' attribute.
#' The term is computed in days, the number of days between the two dates
#' according to \code{calendar}.
#' 
#' @param units a valid term unit: \code{"days"}, \code{"months"}, 
#' \code{"years"}
#' @param from a date object. Since it uses \code{bizdays} package it accepts
#' many date formats.
#' @param to a date object. Since it uses \code{bizdays} package it accepts
#' many date formats.
#' 
#' @rdname compound
#' @export
compound.spotrate <- function(obj, term, units=c('days', 'months', 'years'), from=NULL, to=NULL, ...) {
	units <- match.arg(units)
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


#' @rdname compound
#' @export
discount.spotrate <- function(obj, term, units=c('days', 'months', 'years'), from=NULL, to=NULL, ...) {
	1/compound(obj, term, units, from, to)
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
	parms <- list(...)
	digits <- if (is.null(parms$digits)) options()$digits else parms$digits
	if (length(x) == 1)
		sub(' +$', '', paste(format(rates(x), digits=digits), compounding(x),
			daycount(x), calendar(x)$name))
	else {
		hdr <- sub(' +$', '', paste(compounding(x), daycount(x), 
			calendar(x)$name))
		paste(hdr, paste(format(rates(x), digits=digits), collapse=' '), sep='\n')
	}
}

#' @export
print.spotrate <- function(x, ...) {
	cat(as.character(x, ...), '\n')
	invisible(x)
}

