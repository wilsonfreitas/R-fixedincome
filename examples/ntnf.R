
library(transmute)
library(httr)
library(dplyr)
library(bizdays)
library(purrr)
library(rlist)

bizdays.options$set(default.calendar = "Brazil/ANBIMA")

tr_ <- transmuter(
  match_regex("^\\d{8}$", function(text, match) as.Date(text, format="%Y%m%d"))
)

url <- modify_url("https://api.morph.io/wilsonfreitas/MorthIO_TitulosPublicos_ANBIMA/data.json",
                  query = list(key = "uIUD3TeyxgIVWhVwsFr0",
                               query = "select * from 'data' where data_referencia in (select max(data_referencia) from 'data') and titulo = 'NTN-F'"))
res <- GET(url)
df <- jsonlite::fromJSON(content(res, as = "text"))
df <- transmute::transmute(tr_, df)

coupon_generation <- function(start_date, end_date, coupon_period, coupon_method) {
  if (coupon_method == "backward") {
    .term <- paste0("-", format(coupon_period))
    x <- seq(end_date, start_date, by = .term)
    sort(x)
  } else if (coupon_method == "forward") {
    seq(start_date, end_date, by = format(coupon_period))
  } else
    stop("Invalid coupon method: ", coupon_method)
}

compute_cash_flow <- function(bond, refdate) UseMethod("compute_cash_flow", bond)

compute_cash_flow.bond <- function(bond, refdate) {
  
  cf_dates <- coupon_generation(refdate, bond$maturity_date, bond$coupon_period, bond$coupon_method)
  coupon_payment <- bond$notional_value * (compound(bond$coupon_rate, bond$coupon_period) - 1)
  
  cf <- data.frame(
    coupon_dates = cf_dates,
    payment_dates = following(cf_dates),
    coupon_payments = coupon_payment,
    amortization = 0
  )
  
  cf$business_days <- bizdays(refdate, cf$payment_dates)
  cf$amortization[nrow(cf)] <- bond$notional_value
  bond$cash_flow <- cf
  bond
}

compute_cash_flow.list <- function(bond, refdate) {
  lapply(bond, compute_cash_flow, refdate)
}

pricing <- function(bond) UseMethod("pricing", bond)

pricing.bond <- function(bond) {
  bond$theoretical_value <- with(bond, {
    payments <- cash_flow$coupon_payments + cash_flow$amortization
    sum( payments * discount(yield_rate, cash_flow$business_days, "days") )
  })
  bond
}

pricing.list <- function(bond) {
  lapply(bond, pricing)
}

fixedratebond <- function(..., .config) {
  # notional_value, issue_date, maturity_date, calendar
  # coupon_method = backward | forward, coupon_rate, coupon_period
  .args <- list(...)
  .args$calendar <- if (! is.null(.args$calendar)) .args$calendar else .config$calendar
  .args$coupon_period <- if (! is.null(.args$coupon_period)) as.term(.args$coupon_period) else .args$coupon_period
  .args$coupon_rate <- if (! is.null(.args$coupon_rate)) {
    if (is.character(.args$coupon_rate))
      as.spotrate(.args$coupon_rate)
    else if (is.numeric(.args$coupon_rate))
      spotrate(.args$coupon_rate, .config$compounding, .config$daycount, .args$calendar)
  } else .args$coupon_rate
  .args$yield_rate <- if (! is.null(.args$yield_rate)) {
    if (is.character(.args$yield_rate))
      as.spotrate(.args$yield_rate)
    else if (is.numeric(.args$yield_rate))
      spotrate(.args$yield_rate, .config$compounding, .config$daycount, .args$calendar)
  } else .args$yield_rate
  .args$coupon_method <- if (! is.null(.args$coupon_method)) .args$coupon_method else .config$coupon_method
  
  structure(.args, class = c("fixedratebond", "bond"))
}

setup_bond <- function(df, ..., .config) {
  .args <- lazyeval::lazy_eval(lazyeval::lazy_dots(...), df)
  .args$MoreArgs <- list(.config = .config)
  .args$FUN <- fixedratebond
  .args$SIMPLIFY <- FALSE
  do.call(mapply, .args)
}

df %>% setup_bond(
  maturity_date = data_vencimento,
  yield_rate = taxa_ind/100, #spotrate(taxa_ind/100, "discrete", "business/252", "Brazil/ANBIMA"),
  issue_date = data_base,
  notional_value = 1000,
  coupon_rate = 0.1, #spotrate(0.1, "discrete", "business/252", "Brazil/ANBIMA"),
  coupon_period = "6 months", #as.term("6 months"),
  coupon_method = "backward",
  calendar = "Brazil/ANBIMA",
  instrument_type = "fixedratebond",
  .config = list(
    compounding = "discrete",
    daycount = "business/252",
    calendar = "Brazil/ANBIMA",
    coupon_method = "backward"
  )
) %>%
  compute_cash_flow(refdate = df$data_referencia[1]) %>%
  pricing() -> bonds

map_dbl(bonds, "theoretical_value")

list.map(bonds, c(t=theoretical_value, n=notional_value))
