
source("examples/utils-functions.R")
source("examples/copomscenarios.R")

di1 <- get_curve_from_web("2022-04-04") |> fixedincome::first("2 years")
cd <- copom_dates[copom_dates > di1@refdate][1:6]
cm <- c(100, 25, 0, 0, 0, 0) / 1e4
interpolation(di1) <- interp_copomscenarios(cd, cm)
plot(di1, use_interpolation = TRUE)

# optimize COPOM moves against a given curve
interp <- fit_interpolation(interp_copomscenarios(cd, cm), di1)
interp@copom_moves * 1e4

# optimize COPOM moves against bonds

fit_interpolation_with_bonds <- function(object, x, bonds) {
  par <- object@copom_moves
  res <- optim(par, function(par, x, .dates) {
    interpolation(x) <- interp_copomscenarios(.dates, par)
    
  }, method = "BFGS", x = x, .dates = object@copom_dates)
  interp_copomscenarios(object@copom_dates, res$par)
}


# di1[[1:100]]


# fixedincome::first(di1, "2 years")

# library(stringr)
# library(rbcb)
# library(purrr)

# selic <- get_series(c(SELIC = 432), start_date = "2022-04-01")

# selic_exp <- get_market_expectations("selic", start_date = "2022-03-25") |>
#     filter(baseCalculo == 0) |>
#     mutate(Reuniao = str_split(Reuniao, "/") |> map_chr(~ paste0(.x[2], .x[1]))) |>
#     arrange(Reuniao)

# cd <- copom_dates[copom_dates > "2022-04-01"]
# mv <- c(selic_exp$Mediana[1] - selic$SELIC[1], selic_exp$Mediana |> diff())
# mv[seq_along(cd)]

# copom_dates |> diff()
# copom_dates |> bizdiff("Brazil/ANBIMA")

# adjust for the first future

# cd <- copom_dates[copom_dates > di1@refdate][1:6]
# cm <- c(100, 100, 50, 50, 0, 0) / 1e4

# t1 <- term(bizdays(di1@refdate, cd, di1@calendar), "days")
# t1 <- c(t1, di1@terms[di1@terms > max(t1)][1])
# t <- t1 - shift(t1, fill = 0)

# acc_moves <- c(0, cumsum(cm))
# comp <- cumprod(compound(as.spotrate(di1[1]) + acc_moves, t))

# ix <- di1@terms > max(t1)
# terms <- c(term(1, "days"), t1, di1@terms[ix])
# prices <- c(compound(di1[1]), comp, compound(di1[ix]))

# plot(terms, prices, type = "b")