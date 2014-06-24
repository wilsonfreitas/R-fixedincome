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
#' @param obj the value of the underlying interest rate
#' @param term the term related to the interest paid
#' @param daycount daycount instance
#' @param calendar calendar instance
#' @param compounding the compounding regime can assume the following values:
#' \code{simple}, \code{compounded} and \code{continuous}
#' @param units units
#' @param from dates
#' @param to dates
#' @param ... extra arguments
#' 
#' @return a spotrate spr
#' @name spotrate-class
NULL

#' @rdname spotrate-class
#' @export
as.spotrate <- function(obj, ...) UseMethod('as.spotrate', obj)

#' @rdname spotrate-class
#' @export
as.spotrate.default <- function(obj, compounding, daycount, calendar=NULL, ...) {
	if (!is.numeric(obj))
	    stop('Invalid given rate:', obj)
	structure(obj, compounding=compounding, daycount=daycount, 
		calendar=calendar, class='spotrate')
}

#' @rdname spotrate-class
#' @export
as.spotrate.character <- function(obj, ...) {
	lapply(strsplit(obj, '\\s+', perl=TRUE), function (x) {
		value <- as.numeric(x[1])
		compounding <- as.compounding(x[2])
		daycount <- as.daycount(x[3])
		as.spotrate(value, compounding, daycount)
	})
}

#' @rdname spotrate-class
#' @export
rates.spotrate <- function(obj, ...) as.numeric(obj)

#' @rdname spotrate-class
#' @export
compounding.spotrate <- function (obj, ...) attr(obj, 'compounding')

#' @rdname spotrate-class
#' @export
daycount.spotrate <- function (obj, ...) attr(obj, 'daycount')

#' @rdname spotrate-class
#' @export
calendar.spotrate <- function (obj, ...) attr(obj, 'calendar')

#' @rdname spotrate-class
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

