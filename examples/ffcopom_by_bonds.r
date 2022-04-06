
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


di1_contracts$spot_price - di1_contracts$notional * discount(di1[[di1_contracts$business_days]])

di1 <- get_curve_from_web("2022-04-01")

