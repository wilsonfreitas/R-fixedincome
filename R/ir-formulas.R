#' https://support.google.com/docs/table/25273?hl=en 
#' 
#' @export
FV <- function(r, n, pmt, pv, begining) {
	bf <- if (begining) (1+r) else 1
	f <- pmt*bf*((compound(r, n, 'years') - 1)/r) + pv*compound(r, n, 'years')
	as.numeric(f)
}

#' @export
PV <- function(r, n, pmt, fv, begining) {
	bf <- if (begining) (1+r) else 1
	p <- -pmt*bf*(1 - discount(r, n, 'years'))/r - fv*discount(r, n, 'years')
	as.numeric(p)
}

#' @export
PVP <- function(r, pmt) as.numeric(pmt/r)

#' @export
RATE <- function(n, pmt, pv, fv, begining) {
	f <- function(r) {
		r <- as.spotrate(r, as.compounding('discrete'), as.daycount('actual/365'))
		PV(r, n, pmt, fv, begining) - pv
	}
	uniroot(f, c(1e-10, 1e10))$root
}

