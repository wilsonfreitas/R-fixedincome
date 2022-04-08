
source("examples/utils-functions.R")
devtools::load_all()
source("examples/copomscenarios.R")

contracts <- get_contracts("2022-04-01")

futures <- contracts |>
  filter(Mercadoria == "DI1") |>
  mutate(
    refdate = as.Date(DataRef),
    symbol = paste0(Mercadoria, Vencimento),
    maturity_date = contract_to_maturity(Vencimento) |>
      following("Brazil/ANBIMA"),
    business_days = bizdays(DataRef, maturity_date, "Brazil/ANBIMA"),
    notional = 100000,
    spot_price = PUAtual
  ) |>
  filter(business_days > 0, business_days <= 252) |>
  select(refdate, symbol, maturity_date, notional, business_days, spot_price)

fit_interpolation_with_bonds <- function(object, x, bonds,
                                         limits = c(-0.02, 0.02)) {
  par <- object@copom_moves

  fn <- function(par, x, .dates) {
    interpolation(x) <- interp_copomscenarios(.dates, par)
    o <- bonds$spot_price - bonds$notional * discount(x[[bonds$business_days]])
    sqrt(sum(o^2, na.rm = TRUE))
  }

  res <- optim(par, fn,
    method = "L-BFGS-B",
    lower = limits[1], upper = limits[2],
    control = list(maxit = 1000, trace = TRUE),
    x = x, .dates = object@copom_dates
  )

  interp_copomscenarios(object@copom_dates, res$par)
}

di1 <- get_curve_from_web("2022-04-01") |> fixedincome::first("2 years")

cd <- copom_dates[copom_dates > di1@refdate][1:6]
cm <- c(0, 0, 0, 0, 0, 0) / 1e4

interp <- fit_interpolation_with_bonds(
  interp_copomscenarios(cd, cm),
  di1, futures
)
interp@copom_moves * 1e4

interpolation(di1) <- interp
plot(di1, use_interpolation = TRUE)