
setClass(
  "COPOMScenarios",
  slots = c(
    copom_dates = "ANY",
    copom_moves = "numeric"
  ),
  contains = "Interpolation"
)

interp_copomscenarios <- function(copom_dates, copom_moves) {
  obj <- new("COPOMScenarios", "copomscenarios",
    copom_dates = copom_dates,
    copom_moves = copom_moves
  )
  obj@propagate <- FALSE
  obj
}

setMethod(
  "prepare_interpolation",
  signature(object = "COPOMScenarios", x = "SpotRateCurve"),
  function(object, x, ...) {
    x@interpolation <- NULL

    t1 <- term(bizdays(x@refdate, object@copom_dates, x@calendar), "days")
    t1 <- c(t1, x@terms[x@terms > max(t1)][1])
    t <- t1 - shift(t1, fill = 0)

    acc_moves <- c(0, cumsum(object@copom_moves))
    comp <- cumprod(compound(as.spotrate(x[1]) + acc_moves, t))

    ix <- x@terms > max(t1)
    terms <- c(term(1, "days"), t1, x@terms[ix])
    prices <- c(compound(x[1]), comp, compound(x[ix]))

    interp_coords <- xy.coords(terms, log(prices))
    interp_fun <- approxfun(interp_coords, method = "linear")
    dc <- x@daycount
    comp <- x@compounding
    object@func <- function(term) {
      log_price <- interp_fun(term)
      price <- exp(log_price)
      rates(comp, toyears(dc, term, "days"), price)
    }
    object
  }
)

setMethod(
  "fit_interpolation",
  signature(object = "COPOMScenarios", x = "SpotRateCurve"),
  function(object, x, ...) {
    par <- object@copom_moves
    res <- optim(par, function(par, x, .dates) {
      interpolation(x) <- interp_copomscenarios(.dates, par)
      interpolation_error(x)
    }, method = "BFGS", x = x, .dates = object@copom_dates)
    interp_copomscenarios(object@copom_dates, res$par)
  }
)
