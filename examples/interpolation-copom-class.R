
#' @export
setClass(
  "FlatForwardCOPOM",
  slots = c(
    copom_dates = "ANY",
    conflicts = "character"
  ),
  contains = "Interpolation"
)

#' @export
interp_flatforwardcopom <- function(copom_dates, conflicts) {
  new("FlatForwardCOPOM", "flatforwardcopom",
      copom_dates = copom_dates,
      conflicts = conflicts)
}

split_curve_into_copom_dates <- function(curve, copom_dates) {
  curve_fwd <- forwardrate(curve)
  dates_terms <- maturities(curve)
  lapply(seq_along(copom_dates), function(x) {
    if (x == 1) {
      idx <- which(dates_terms >= copom_dates[x] & dates_terms <= copom_dates[x+1])
      seed_rate <- as.numeric(curve[1])
    } else if (!is.na(copom_dates[x+1])) {
      idx <- which(dates_terms >= copom_dates[x] & dates_terms <= copom_dates[x+1])
      seed_rate <- NA_real_
    } else {
      idx <- which(dates_terms >= copom_dates[x])
      seed_rate <- NA_real_
    }
    list(
      copom_date = copom_dates[x],
      futures = curve[idx],
      forward = curve_fwd[idx],
      seed_rate = seed_rate
    )
  })
}

calc_zero <- function(last_result, du_copom, futs, seed_rate) {
  if (is.null(last_result)) {
    spotratecurve(
      seed_rate,
      du_copom,
      refdate = futs@refdate,
      .copyfrom = futs
    )
  } else {
    # this is the forward rate that starts at the last copom date
    fwd_copom <- last_result$copom_forward
    # replace the terms to extend it up to the next copom date
    fwd_copom@terms <- du_copom - last_result$zero@terms
    fwd_rates <- forwardrate(last_result$zero)
    # compose the zero with the rates up to the last copom date and
    # from the last to the next copom date
    fwd_rates <- c(fwd_rates, fwd_copom)
    spot_curve <- as.spotratecurve(fwd_rates, last_result$zero@refdate)
    spot_curve[[du_copom]]
  }
}

copom_calc <- function(parts, x = 1, results = NULL,
                       conflicts = c("forward", "second", "first", "optimize")) {
  
  if (x > length(parts)) {
    zero_curve <- do.call(c, lapply(results, function(x) x$zero))
    return(zero_curve)
  }
  
  conflicts <- match.arg(conflicts)
  futs <- parts[[x]]$futures
  result <- if (length(futs) == 2) {
    if (conflicts == "forward") {
      calc_with(parts, x, results, forward_calc_use_forward)
    } else if (conflicts == "first") {
      calc_with(parts, x, results, forward_calc_use_first_future)
    } else if (conflicts == "second") {
      calc_with(parts, x, results, forward_calc_use_second_future)
    } else if (conflicts == "optimize") {
      calc_with(parts, x, results, forward_calc_optim)
    }
  } else if (length(futs) == 0) {
    NULL
  } else {
    calc_with(parts, x, results, forward_calc_use_first_future)
  }
  results[[length(results) + 1]] <- result
  copom_calc(parts, x+1, results, conflicts)
}

calc_with <- function(parts, x, results, forward_calc) {
  copom_date <- parts[[x]]$copom_date
  futs <- parts[[x]]$futures
  fwds <- parts[[x]]$forward
  refdate <- futs@refdate
  du_copom <- term(bizdays(refdate, copom_date, "Brazil/ANBIMA"))
  
  last_result <- results[[length(results)]]
  
  zero <- calc_zero(last_result, du_copom, futs, parts[[x]]$seed_rate)
  fwd <- forward_calc(futs, fwds, du_copom, zero)

  list(
    copom_date = copom_date,
    zero = zero,
    copom_forward = fwd
  )
}

forward_calc_use_first_future <- function(futs, fwds, du_copom, zero) {
  futs[[du_copom]] <- zero
  idx <- match(du_copom, futs@terms)
  forwardrate(futs, du_copom, futs@terms[idx+1])
}

forward_calc_use_second_future <- function(futs, fwds, du_copom, zero) {
  futs <- futs[-1]
  futs[[du_copom]] <- zero
  idx <- match(du_copom, futs@terms)
  forwardrate(futs, du_copom, futs@terms[idx+1])
}

forward_calc_use_forward <- function(futs, fwds, du_copom, zero) {
  fwds[2]
}

forward_calc_optim <- function(futs, fwds, du_copom, zero) {
  du_fwd <- futs@terms - du_copom
  f_obj <- function(x) {
    spot_rate <- spotratecurve(rep(x, length(du_fwd)), du_fwd,
                               refdate = zero@refdate,
                               .copyfrom = futs)
    fact_obj <- compound(zero) * compound(spot_rate)
    sum(compound(futs) - fact_obj) ^ 2
  }
  res <- optim(as.numeric(fwds[2]), f_obj, method = "Brent",
               lower = 0, upper = 1)
  forwardrate(res$par, du_fwd, .copyfrom = futs)
}

#' @export
setMethod(
  "prepare_interpolation",
  signature(object = "FlatForwardCOPOM", x = "SpotRateCurve"),
  function(object, x, ...) {
    x@interpolation <- NULL
    
    parts <- split_curve_into_copom_dates(x, object@copom_dates)
    zero_curve <- copom_calc(parts, 1, conflicts = object@conflicts)
    zero_curve <- c(x[1], zero_curve, x[x@terms > max(zero_curve@terms)])
    
    terms <- as.numeric(zero_curve@terms)
    prices <- compound(zero_curve)
    interp.coords <- xy.coords(terms, log(prices))
    interp.FUN <- approxfun(interp.coords, method='linear')
    dc <- zero_curve@daycount
    comp <- zero_curve@compounding
    object@func <- function (term) {
      log.price <- interp.FUN(term)
      price <- exp(log.price)
      rates(comp, timefactor(dc, term, "days"), price)
    }
    object
  }
)

