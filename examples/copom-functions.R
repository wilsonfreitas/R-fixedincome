
library(glue)
library(xml2)
library(stringr)
library(httr)
library(ggplot2)
library(dplyr)
library(bizdays)
library(forcats)
library(rbcb)
library(fixedincome)


copom_dates <- c("02/02/2022", "16/03/2022", "04/05/2022", "15/06/2022",
                 "03/08/2022", "21/09/2022", "26/10/2022", "07/12/2022",
                 "20/01/2021", "17/03/2021", "05/05/2021", "16/06/2021",
                 "04/08/2021", "22/09/2021", "27/10/2021", "08/12/2021",
                 "05/02/2020", "18/03/2020", "06/05/2020", "17/06/2020",
                 "05/08/2020", "16/09/2020", "28/10/2020", "09/12/2020",
                 "06/02/2019", "20/03/2019", "08/05/2019", "19/06/2019",
                 "31/07/2019", "18/09/2019", "30/10/2019", "11/12/2019",
                 "07/02/2018", "21/03/2018", "16/05/2018", "20/06/2018",
                 "01/08/2018", "19/09/2018", "31/10/2018", "12/12/2018",
                 "11/01/2017", "22/02/2017", "12/04/2017", "31/05/2017",
                 "26/07/2017", "06/09/2017", "25/10/2017", "06/12/2017",
                 "20/01/2016", "02/03/2016", "27/04/2016", "08/06/2016",
                 "20/07/2016", "31/08/2016", "19/10/2016", "29/11/2016",
                 "21/01/2015", "04/03/2015", "29/04/2015", "03/06/2015",
                 "29/07/2015", "02/09/2015", "21/10/2015", "25/11/2015") |>
  as.Date("%d/%m/%Y") |> 
  sort()

# functions ----

copom_curve_term <- function(copom_date,
                             zero,
                             copom_forward,
                             move) {
  data.frame(
    copom_date = copom_date,
    zero = zero,
    copom_forward = copom_forward,
    move = move
  )
}

calc_first_term <- function(parts, x, results) {
  copom_date <- parts[[x]]$copom_date
  futs <- parts[[x]]$futures
  fwds <- parts[[x]]$forward
  refdate <- futs@refdate
  du_copom <- term(bizdays(refdate, copom_date, "Brazil/ANBIMA"))
  
  du_rem <- futs@terms[length(futs@terms)] - du_copom
  
  if (du_rem > 0) {
    zero <- spotratecurve(
      as.numeric(futs[1]),
      du_copom,
      refdate = futs@refdate,
      .copyfrom = futs
    )
    futs[[du_copom]] <- zero
    fwd <- forwardrate(futs, du_copom, futs@terms[length(futs@terms)])
    copom_curve_term(
      copom_date = copom_date,
      zero = zero,
      copom_forward = fwd,
      move = 1e4 * (as.numeric(fwd) - as.numeric(futs[1]))
    )
  } else {
    fwd <- 10.65/100
    copom_curve_term(
      copom_date = copom_date,
      zero = as.numeric(futs[1]),
      copom_forward = fwd,
      move = 1e4 * (fwd - as.numeric(futs[1]))
    )
  }
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
    moves <- do.call(c, lapply(results, function(x) x$move))
    zero_curve <- do.call(c, lapply(results, function(x) x$zero))
    forward_rates <- do.call(c, lapply(results, function(x) x$copom_forward))
    forward_curve <- spotratecurve(
      as.numeric(forward_rates),
      zero_curve@terms,
      .copyfrom = zero_curve,
      refdate = zero_curve@refdate
    )
    
    zero_df <- zero_curve |> as.data.frame()
    forward_df <- forward_curve |> as.data.frame()
    
    df <- merge(zero_df, forward_df,
                by = c("terms", "dates"),
                suffixes = c("_zero", "_forward")) |>
      arrange(terms) |>
      mutate(move = moves)
    
    return(df)
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
  move <- if (is.null(last_result)) {
    1e4 * (as.numeric(fwd) - as.numeric(zero))
  } else {
    1e4 * (as.numeric(fwd) - as.numeric(last_result$copom_forward))
  }
  
  copom_curve_term(
    copom_date = copom_date,
    zero = zero,
    copom_forward = fwd,
    move = move
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

