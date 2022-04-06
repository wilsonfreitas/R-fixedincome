
source("examples/utils-functions.R")

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


di1 <- get_curve_from_web("2022-04-04") |> fixedincome::first("2 years")
cd <- copom_dates[copom_dates > di1@refdate][1:6]
cm <- c(100, 25, 0, 0, 0, 0) / 1e4
interpolation(di1) <- interp_copomscenarios(cd, cm)
di1[[1:100]]
plot(di1, use_interpolation = TRUE)


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