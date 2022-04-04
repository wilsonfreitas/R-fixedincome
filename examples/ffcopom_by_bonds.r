
source("examples/utils-functions.R")

contracts <- get_contracts("2022-03-10")

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
    select(refdate, symbol, maturity_date, notional, business_days, spot_price)

di1 <- get_curve_from_web("2022-03-10")

di1_contracts$spot_price - di1_contracts$notional * discount(di1[[di1_contracts$business_days]])

setClass(
    "COPOMScenarios",
    slots = c(
        copom_dates = "ANY",
        copom_moves = "numeric"
    ),
    contains = "Interpolation"
)

interp_copomscenarios <- function(copom_dates, copom_moves) {
    new("COPOMScenarios", "copomscenarios",
        copom_dates = copom_dates,
        copom_moves = copom_moves
    )
}

setMethod(
    "prepare_interpolation",
    signature(object = "COPOMScenarios", x = "SpotRateCurve"),
    function(object, x, ...) {
        x@interpolation <- NULL

        du_1 <- bizdays(x@refdate, object@copom_dates, "Brazil/ANBIMA")
        du_2 <- shift(du_1, fill = 0)
        t <- term(du_1 - du_2, "days")

        first_vert <- as.spotrate(x[1])
        acc_moves <- shift(cumsum(object@copom_moves), fill = 0)
        rates <- first_vert + acc_moves
        comp <- cumprod(compound(rates, t))

        ix <- x@terms > max(t)
        comp2 <- compound(x[ix])

        terms <- c(t, x@terms[ix])
        prices <- compound(c(comp, comp2))
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