

source("./examples/utils-functions.R")
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

# ZeroCouponBond(
#   Name = "LTN_20110101",
#   Currency = "BRL",
#   Daycount = "business/252",
#   Calendar = "Brazil/ANBIMA",
#   DiscountCurve = Curve("curve_name"),
#   Notional = 1000,
#   MaturityDate = as.Date("2025-01-01"),
#   MaturityDateAdjustment = "following",
#   SpotPrice = 719.14,
#   YieldRate = "0.1247 discrete business/252 Brazil/ANBIMA",
#   TheoPrice = PricingFunction("function_name")
# )

# ZeroCouponBond(
#   Name = "DI1F21",
#   Currency = "BRL",
#   Daycount = "business/252",
#   Calendar = "Brazil/ANBIMA",
#   CalendarAdjustment = "following",
#   DiscountCurve = Curve("curve_name"),
#   Notional = 1000,
#   MaturityDate = as.Date("2025-01-01"),
#   SpotPrice = 719.14,
#   YieldRate = "0.1247 discrete business/252 Brazil/ANBIMA",
#   TheoPrice = PricingFunction("function_name")
# )

check_attributes <- function(.data, attribute_name, default_value) {
    if (!exists(attribute_name, .data)) {
        if (is.vector(default_value)) {
            .data[[attribute_name]] <- default_value
        } else {
            attr(.data, attribute_name) <- default_value
        }
    }
    .data
}

DI1Swap <- function(.data, ...) {
    .data_fini <- .data |>
        mutate(Type = "DI1Swap", ...)

    .data_fini <- check_attributes(.data_fini, "Notional", 1e5)
    .data_fini <- check_attributes(.data_fini, "Currency", "BRL")
    .data_fini <- check_attributes(.data_fini, "Daycount", "business/252")
    .data_fini <- check_attributes(.data_fini, "Calendar", "Brazil/ANBIMA")
    .data_fini <- check_attributes(.data_fini, "CalendarAdjustment", "following")
    .data_fini <- check_attributes(
        .data_fini, "AdjustedQuoteRate",
        function(x) {
            print(x)
        }
    )

    if (!exists("Name", .data_fini)) {
        stop("All fini classes must have *name* attribute.")
    }

    valid_attributes <- c(
        "Type", "Name", "Currency", "AdjustedQuoteRate", "Daycount",
        "Calendar", "CalendarAdjustment",
        "MaturityDate", "Notional", "SpotPrice"
    )

    .data_fini <- .data_fini[valid_attributes]

    class(.data_fini) <- c("future_di1", "fini", class(.data_fini))
    .data_fini
}

`$.fini` <- function(x, attribute_name) {
    if (exists(attribute_name, x)) {
        if (is(x[[attribute_name]], "PricingCommand")) {
            execute(x[[attribute_name]], x)
        } else {
            x[[attribute_name]]
        }
    } else {
        if (attribute_name == "TheoreticalValue") {
            theoretical_value(x)
        } else {
            stop("Attribute ", attribute_name, " does not exist.")
        }
    }
}

df <- get_contracts(offset(Sys.Date(), -1, "Brazil/ANBIMA"))

futures <- df |>
    filter(Mercadoria == "DI1") |>
    DI1Swap(
        Name = paste0(Mercadoria, Vencimento),
        MaturityDate = maturity2date(Vencimento, "first day"),
        SpotPrice = PUAtual,
        StrikePrice = PUAtual,
    )
