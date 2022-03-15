

source("examples/utils-functions.R")
library(fixedincome)
library(dplyr)
library(bizdays)
library(stringr)
# fini package

maturity2date <- function(x, expr = "first day") {
  year <- as.integer(str_sub(x, 2)) + 2000
  month <- str_pad(code2month(str_sub(x, 1, 1)), 2, pad = "0")
  getdate(expr, paste0(year, "-", month))
}

code2month <- function(x) {
  m <- c(
    F = 1, G = 2, H = 3, J = 4, K = 5, M = 6,
    N = 7, Q = 8, U = 9, V = 10, X = 11, Z = 12
  )
  m[x]
}

ZeroCouponBond(
  Name = "LTN_20110101",
  Currency = "BRL",
  Daycount = "business/252",
  Calendar = "Brazil/ANBIMA",
  DiscountCurve = Curve("curve_name"),
  Notional = 1000,
  MaturityDate = as.Date("2025-01-01"),
  MaturityDateAdjustment = "following",
  SpotPrice = 719.14,
  YieldRate = "0.1247 discrete business/252 Brazil/ANBIMA",
  TheoPrice = PricingFunction("function_name")
)

ZeroCouponBond(
  Name = "DI1F21",
  Currency = "BRL",
  Daycount = "business/252",
  Calendar = "Brazil/ANBIMA",
  CalendarAdjustment = "following",
  DiscountCurve = Curve("curve_name"),
  Notional = 1000,
  MaturityDate = as.Date("2025-01-01"),
  SpotPrice = 719.14,
  YieldRate = "0.1247 discrete business/252 Brazil/ANBIMA",
  TheoPrice = PricingFunction("function_name")
)

df <- get_contracts(offset(Sys.Date(), -1, "Brazil/ANBIMA"))

futures <- df |>
  filter(Mercadoria == "DI1") |>
  DI1Swap(
    Name = paste0(Mercadoria, Vencimento),
    MaturityDate = maturity2date(Vencimento, "first day"),
    Currency = "BRL",
    Daycount = "business/252",
    Calendar = "Brazil/ANBIMA",
    CalendarAdjustment = "following",
    Notional = 100000,
    SpotPrice = PUAtual,
    StrikePrice = PUAtual,
    YieldRate = spotrate(0, "discrete", "business/252", "Brazil/ANBIMA")
  )

DI1Swap <- function(.data, ...) {
  .data |>
    mutate(Type = "DI1Swap", ...) |>
    select(
      Type, Name, MaturityDate, Currency, YieldRate, Daycount, Calendar,
      CalendarAdjustment, Notional, SpotPrice
    )
}